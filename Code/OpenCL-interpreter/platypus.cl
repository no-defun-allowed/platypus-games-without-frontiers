/* -*- mode: opencl -*- */

typedef uint player;
typedef uint tape;
enum colour { GREEN, YELLOW };
enum animal { PLATYPUS, WOMBAT, EMU, KANGAROO };
enum direction { WATTLE, GHOST_GUM };

/* Decode machines */
/* Common Lisp LDB */
#define LDB(size, position, n) (((n) >> (position)) & ((1 << (size)) - 1))
#define TRANS(player, read, state) LDB(4, 4 * ((read) + (state) * 2 - 1), (player))
#define COLOUR(trans) LDB(1, 0, trans)
#define ANIMAL(trans) LDB(2, 1, trans)
#define DIRECTION(trans) LDB(1, 3, trans)

#define BOARD_SIZE 21
// All yellow
#define INITIAL_BOARD ((1 << BOARD_SIZE) - 1)
#define INITIAL_POS (BOARD_SIZE / 2)
#define HIGHEST_POS (BOARD_SIZE - 1)
#define MOVE_LIMIT 50

#define BIT(pos, n) (1 & ((n) >> (pos)))
#define PUT_BIT(pos, bit, n) ((bit) << (pos) | ((n) & ~(1 << (pos))))

/* Lotsa pointers, but after act() gets inlined they all go away. */
void move(enum colour read, player p, uint weight,
          uint *tape, uint *pos, enum animal *a,
          uint *score, bool *running) {
  uint transition = TRANS(p, read, *a);
  // printf("player %x: %d %d reads %d\n", p, *pos, *a, read);
  if (read == GREEN && *a == PLATYPUS) *running = false;
  /* Preserve score if we're not running anymore. Also scale
   * by weight to avoid a multiply when adding to the tally. */
  *score += (COLOUR(transition) != read && *running) ? weight : 0;
  *tape = PUT_BIT(*pos, COLOUR(transition), *tape);
  *a = ANIMAL(transition);
  /* Check for wrapping around mod BOARD_SIZE. */
  if (DIRECTION(transition) == WATTLE)
    *pos = (*pos == 0) ? HIGHEST_POS : (*pos - 1);
  else
    *pos = (*pos == HIGHEST_POS) ? 0 : (*pos + 1);
}

__kernel void platypus(uint player1,
                       __global player *players, __global uint *weights,
                       uint player_count, uint chunk_size, __global ulong *results) {
  /* Allocator state */
  uint cursor = chunk_size * get_global_id(0);
  uint limit = cursor + chunk_size;
  if (limit > player_count) limit = player_count;
  /* Tallies */
  uint wins = 0, score = 0;
  /* Interpreter state */
  uint pos1, pos2, score1 = 0, score2 = 0, moves, weight; enum animal a1, a2;
  tape tape; player player2;
  bool running = false;
  while (true) {
    /* Refill */
    if (!running) {
      /* Update tallies */
      wins += score1 > score2 ? weight : 0;
      /* score1 gets scaled already by act. */
      score += score1;
      if (cursor < limit) {
        /* Reset values */
        running = true;
        player2 = players[cursor]; weight = weights[cursor];
        pos1 = pos2 = INITIAL_POS;
        a1 = a2 = KANGAROO;
        score1 = score2 = moves = 0;
        tape = INITIAL_BOARD;
        cursor++;
      } else {
        /* No more work */
        break;
      }
    }
    
    /* Step players */
    enum colour read;
    read = BIT(pos1, tape);
    move(read, player1, weight, &tape, &pos1, &a1, &score1, &running);
    read = BIT(pos2, tape);
    move(read, player2, weight, &tape, &pos2, &a2, &score2, &running);
    if (++moves == MOVE_LIMIT) running = false;
  }
  atom_add(&results[0], wins);
  atom_add(&results[1], score);
}
