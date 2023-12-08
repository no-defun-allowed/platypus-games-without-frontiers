import math
import numpy as np
import pyopencl as cl
import sys

CHUNK_SIZE = 4
mf = cl.mem_flags

with open("platypus.cl", "r") as f:
    CODE = f.read()

def load(players, weights):
    global ctx, queue, program, player_count, player_buffer, weight_buffer, chunks
    ctx = cl.create_some_context()
    queue = cl.CommandQueue(ctx)
    program = cl.Program(ctx, CODE).build()
    # Make buffers
    player_count = len(players)
    player_buffer = cl.Buffer(ctx, mf.READ_WRITE | mf.COPY_HOST_PTR, hostbuf=players)
    weight_buffer = cl.Buffer(ctx, mf.READ_WRITE | mf.COPY_HOST_PTR, hostbuf=weights)
    chunks = math.ceil(player_count / CHUNK_SIZE)

def run(player1):
    sys.stdout.flush()
    results = np.zeros(2, dtype=np.uint64)
    result_buffer = cl.Buffer(ctx, mf.READ_WRITE | mf.COPY_HOST_PTR, hostbuf=results)
    program.platypus(queue,
                     [chunks],
                     None,
                     np.uint32(player1),
                     player_buffer, weight_buffer,
                     np.uint32(player_count),
                     np.uint32(CHUNK_SIZE),
                     result_buffer)
    cl.enqueue_copy(queue, results, result_buffer)
    return int(results[0]), int(results[1])

if __name__ == "__main__":
    import argparse
    import random
    import time
    parser = argparse.ArgumentParser(prog="platypus.py",
                                     description="Interpret Platypus games on a GPU")
    parser.add_argument("count", type=int)
    parser.add_argument("machine_file")
    parser.add_argument("weight_file")
    args = parser.parse_args()
    load(np.load(args.machine_file), np.load(args.weight_file))
    start = time.time()
    for _ in range(args.count):
        run(random.randint(0, 2 ** 28))
    end = time.time()
    throughput = (args.count) * player_count / (end - start)
    print("")
    print(f"{throughput:.0f} matches/second")
    print(f"It would take {2 * (player_count ** 2) / throughput / 86400:.1f} days to run a full Platypus tournament.")
