# YANG Data Model parser
def parsing(line,id):
  linelen = len(line)
  skip = False

  # estimate level
  start_i = 0
  while line[start_i] == ' ' or line[start_i] == '|':
    start_i += 1
  level = int((start_i-2)/3)

  # check out the start point for extracting field
  if line[start_i+3] == 'r':
    start_i += 6
  else:
    start_i += 4
  end_i = start_i

  # check out the end point for extracting field
  # ( exception
  if line[start_i] == '(':
    for i in range(start_i, linelen):
      if line[i] == ')':
        end_i = i
        break
    field = line[start_i+1:end_i]
    start_i = end_i+1
    skip = True

  # general situation
  else:
    for i in range(start_i, linelen):
      if line[i] == '*' or line[i] == '?' or line[i] == ' ' or line[i] == '\n':
        end_i = i
        break
    field = line[start_i:end_i]
    if line[end_i] == '*' or line[end_i] == '?':
      start_i = end_i+1
    else:
      start_i = end_i

  # check out the type of CFG node
  isTerminal = True
  end_i = start_i
  while line[end_i] == ' ':
    end_i += 1
  if line[end_i] == '[' or line[end_i] == '\n':
    isTerminal = False

  return [skip, level, field, isTerminal, id]
