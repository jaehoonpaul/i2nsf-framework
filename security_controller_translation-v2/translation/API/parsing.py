# YANG Data Model parser
def parsing(line):
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

  return [skip, level, field, isTerminal]

# Convert JSON to XML
def json2xml(json_obj, line_padding=""):
  result_list = list()
  json_obj_type = type(json_obj)
  if json_obj_type is list:
    for sub_elem in json_obj:
      result_list.append(json2xml(sub_elem, line_padding))
    return "\n".join(result_list)
  if json_obj_type is dict:
    for tag_name in json_obj:
      sub_obj = json_obj[tag_name]
      result_list.append("%s<%s>" % (line_padding, tag_name))
      result_list.append(json2xml(sub_obj, "\t" + line_padding))
      result_list.append("%s</%s>" % (line_padding, tag_name))
    return "\n".join(result_list)
  return "%s%s" % (line_padding, json_obj)

