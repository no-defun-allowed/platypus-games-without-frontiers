import pyopencl as cl

CODE = """
uint scan_sum(uint in) {
  /* Inclusive scan sum c.f. Hillis-Steele */
  /* We shouldn't really be allowed to have __local here, but the AMD GPU OpenCL allows it. */
  __local uint temps[2][64];
  uint id = (uint)get_local_id(0);
  temps[0][id] = in;
  barrier(CLK_LOCAL_MEM_FENCE);
#define STEP(target, source, amount) target[id] = source[id] + (id < amount ? 0 : source[id - amount]); barrier(CLK_LOCAL_MEM_FENCE)
  STEP(temps[1], temps[0], 1);
  STEP(temps[0], temps[1], 2);
  STEP(temps[1], temps[0], 4);
  STEP(temps[0], temps[1], 8);
  STEP(temps[1], temps[0], 16);
  return temps[1][id] + (id < 32 ? 0 : temps[1][id - 32]);
}

__kernel void test_kernel() {
  printf("local %ld has scan sum = %d\\n", get_local_id(0), scan_sum(get_local_id(0) % 2));
}
"""

ctx = cl.create_some_context()
queue = cl.CommandQueue(ctx)
program = cl.Program(ctx, CODE).build()

program.test_kernel(queue, [64], [64])
