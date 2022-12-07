# --- Day 5: Supply Stacks ---

# The expedition can depart as soon as the final supplies have been unloaded from the ships. Supplies are stored in stacks of marked crates, but because the needed supplies are buried under many other crates, the crates need to be rearranged.

# The ship has a giant cargo crane capable of moving crates between stacks. To ensure none of the crates get crushed or fall over, the crane operator will rearrange them in a series of carefully-planned steps. After the crates are rearranged, the desired crates will be at the top of each stack.

# The Elves don't want to interrupt the crane operator during this delicate procedure, but they forgot to ask her which crate will end up where, and they want to be ready to unload them as soon as possible so they can embark.

# They do, however, have a drawing of the starting stacks of crates and the rearrangement procedure (your puzzle input). For example:

    # [D]    
# [N] [C]    
# [Z] [M] [P]
 # 1   2   3 

# move 1 from 2 to 1
# move 3 from 1 to 3
# move 2 from 2 to 1
# move 1 from 1 to 2
# In this example, there are three stacks of crates. Stack 1 contains two crates: crate Z is on the bottom, and crate N is on top. Stack 2 contains three crates; from bottom to top, they are crates M, C, and D. Finally, stack 3 contains a single crate, P.

# Then, the rearrangement procedure is given. In each step of the procedure, a quantity of crates is moved from one stack to a different stack. In the first step of the above rearrangement procedure, one crate is moved from stack 2 to stack 1, resulting in this configuration:

# [D]        
# [N] [C]    
# [Z] [M] [P]
 # 1   2   3 
# In the second step, three crates are moved from stack 1 to stack 3. Crates are moved one at a time, so the first crate to be moved (D) ends up below the second and third crates:

        # [Z]
        # [N]
    # [C] [D]
    # [M] [P]
 # 1   2   3
# Then, both crates are moved from stack 2 to stack 1. Again, because crates are moved one at a time, crate C ends up below crate M:

        # [Z]
        # [N]
# [M]     [D]
# [C]     [P]
 # 1   2   3
# Finally, one crate is moved from stack 1 to stack 2:

        # [Z]
        # [N]
        # [D]
# [C] [M] [P]
 # 1   2   3
# The Elves just need to know which crate will end up on top of each stack; in this example, the top crates are C in stack 1, M in stack 2, and Z in stack 3, so you should combine these together and give the Elves the message CMZ.

# After the rearrangement procedure completes, what crate ends up on top of each stack?

#### SOLUTION ####
library(stringr)

input = read_lines('day-05-input.txt')
stacks = input[1:8]
steps = input[11:511]

# initialize each stack
# the bottom (8th) stack will contain the positions of the letters
slots = as.data.frame(str_locate_all(stacks[8], '[A-Z]')[[1]])$start

for(slot in 1:length(slots)) {
  eval(parse(text = glue::glue("stack{slot} = sapply(stacks, function(x) substr(x, slots[{slot}], slots[{slot}])) |> str_flatten() |> str_remove_all(' ') |> str_split('') |> unlist() |> rev() |> str_flatten()")))
}

crate_mover = 9001

for(i in 1:length(steps)) {
  
  step = steps[i]
  
  number = str_extract(step, 'move \\d+') |> str_extract_all('\\d+') |> as.numeric()
  from = str_extract(step, 'from \\d+') |> str_extract_all('\\d+') |> as.numeric()
  to = str_extract(step, 'to \\d+') |> str_extract_all('\\d+') |> as.numeric()
  
  if(crate_mover == 9000) {
    eval(parse(text = glue::glue("move = str_sub(stack{from}, nchar(stack{from}) - {number} + 1, nchar(stack{from})) |> str_split('') |> unlist() |> rev() |> str_flatten()")))    
  }

  if(crate_mover == 9001) {
    eval(parse(text = glue::glue("move = str_sub(stack{from}, nchar(stack{from}) - {number} + 1, nchar(stack{from}))")))    
  }
  
  eval(parse(text = glue::glue('stack{from} = str_sub(stack{from}, 1, nchar(stack{from}) - {number})')))
  eval(parse(text = glue::glue("stack{to} = paste0(stack{to}, '{move}')")))
}

top_slots = character()

for(slot in 1:length(slots)) {
  eval(parse(text = glue::glue("top_slots[{slot}] = str_sub(stack{slot},nchar(stack{slot}), nchar(stack{slot}))")))
}

str_flatten(top_slots)
