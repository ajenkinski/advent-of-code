#=
https://adventofcode.com/2020/day/8
=#

struct Instruction
  op_code::String
  arg::Int
end

"Virtual machine state"
mutable struct Machine
  program::Array{Instruction,1}

  "Program counter: index into program of next instruction to execute"
  pc::Int

  accumulator::Int
end

input = map(readlines("day8-input.txt")) do line
  m = match(r"^(\S+)\s+([+-]?\d+)$", line)
  @assert m != nothing
  Instruction(m.captures[1], parse(Int, m.captures[2]))
end

"Execute the program contained in instructions in a fresh virtual machine, and return the resulting machine state.
Execution stops when an instruction is about to be repeated or the program counter goes out of bounds.  Return value is
a (machine, bool) tuple, where the bool indicates whether the program exited normally, where normal is defined by the
program counter ending up at one past the end of the instructions."
function exec_program(instructions)
  machine = Machine(instructions, 1, 0)

  seen = Set{Int}()

  while machine.pc ∉ seen && machine.pc ∈ 1:length(machine.program)
    push!(seen, machine.pc)
    ins = machine.program[machine.pc]

    pc_inc = 1

    if ins.op_code == "acc"
      machine.accumulator += ins.arg
    elseif ins.op_code == "jmp"
      pc_inc = ins.arg
    elseif ins.op_code == "nop"
      # do nothing
    else
      error("Unknown opcode: \"$(ins.op_code)\"")
    end

    machine.pc += pc_inc
  end

  return (machine, machine.pc == length(machine.program) + 1)
end

machine, normal_exit = exec_program(input)
part1_answer = machine.accumulator
print("Part 1: After execution accumulator = $(part1_answer)\n")

# part 2.  Get program to exit normally instead of infinitely looping by changing one jmp to nop or one nop to jmp.
# Report the accumulator value after normal exit.

# I'm assuming it starts out not exiting normally, but just verify
machine, normal_exit = exec_program(input)
@assert !normal_exit

for (i, ins) in pairs(input)
  if ins.op_code in ["jmp", "nop"]
    new_ins = Instruction(ins.op_code == "nop" ? "jmp" : "nop", ins.arg)
    new_input = copy(input)
    new_input[i] = new_ins
    global machine, normal_exit = exec_program(new_input)
    if normal_exit
      break
    end
  end
end

@assert normal_exit
print("Part 2: accumulator after fixing program: $(machine.accumulator)\n")
