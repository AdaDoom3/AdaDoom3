## textures/base_door/light_panel1
## {
##   qer_editorimage textures/base_door/stedoorframe2.tga
##   bumpmap   textures/base_door/stedoorframe2_local.tga
##   diffusemap    textures/base_door/stedoorframe2_d.tga
##   specularmap   textures/base_door/stedoorframe2_s.tga
##   {
##     if ( parm7 == 0 )
##     blend   add
##     map   textures/base_door/stedoorframered_add.tga
##     rgb   5
##   }
##   {
##     if ( parm7 == 1 )
##     blend   add
##     map   textures/base_door/stedoorframegrn_add.tga
##     rgb   5
##   }
## }

import fileinput
import re

##############
## Patterns ##
##############

WHITE_SPACE = r'[\\s]+'
NUM_PATTERN = r'[0-9]' + WHITE_SPACE + r'[-+0-9]'
TEX_PATH = r'(savegames|fonts|textures|guis|ui|guisurfs|particles|lights|models|env)[\\/][a-z_\\/0-9A-Z]*'
re.compile (NUM_PATTERN)
re.compile (WHITE_SPACE)

#############
## Globals ##
#############

mode            = 0
in_material = False
blend_mode = 0
is_blend = False
did_diffuse = False
did_specular = False
did_bumpmap  = False
s = []

##################
## get_material ##
##################

def get_material (line):
  mat_name = re.search (TEX_PATH, line)
  if mat_name:
    mat_name = re.sub (r'[\\/]', '_', mat_name.group (0))
  else:
    mat_name = re.search (r'_[a-z_\\/0-9A-Z]*', line)
    return mat_name.group (0)
  return mat_name
 
##################
## process_line ##
##################

def process_line (line, next_line):
  global mode
  global in_material
  global is_blend
  global blend_mode
  global s

  ## Strip the EOL
  line = line.strip ()
  line = re.sub (r'//.*', '', line)

  if re.search (r'^table', line):
    return

  ## Ignore empty lines
  if not line:
    return

  if re.search (r'{', line):
    s.append ('Crap')

  if re.search (r'}', line):
    if len (s) != 0:
      s.pop ()
      if len (s) == 0 and in_material:
        in_material = False
        print ('}')

  ## See if we are at the start of a material
  if not in_material and re.search (r'^' + TEX_PATH, line):
    in_material = True
    print ('Material')
    print ('{')
    print ('   Name {string {\"' + get_material (line) + '\"}}')

  elif in_material:

    ## A "blended" texture
    if re.search (r'^blend' + WHITE_SPACE, line):

      ## Handle blend modes
      if re.search (r'[dD]iffuse[mM]ap', line):
        is_blend = True
        blend_mode = 0
      elif re.search (r'[sS]pecular[mM]ap', line):
        is_blend = True
        blend_mode = 1
      elif re.search (r'[bB]ump[mM]ap', line):
        is_blend = True
        blend_mode = 2
      else:
        blend_mode = -1

    ## Handle a blended texture and ignore other attributes
    elif is_blend and re.search (r'^[mM]ap' + WHITE_SPACE, line):
      is_blend = False
      if re.search (r'addnormals', line):
        return
      elif blend_mode == 0:
        print ('   Texture (attrib = "diffuse") {string {\"' + get_material (line) + '\"}}')
      elif blend_mode == 1:
        print ('   Texture (attrib = "specular") {string {\"' + get_material (line) + '\"}}')
      elif blend_mode == 2:
        print ('   Texture (attrib = "normal") {string {\"' + get_material (line) + '\"}}')

    ## Normal path for diffuse, specular, and normal textures
    elif re.search (r'^[dD]iffuse[mM]ap', line):
      print ('   Texture (attrib = "diffuse") {string {\"' + get_material (line) + '\"}}')

    elif re.search (r'^[sS]pecular[mM]ap', line):
      print ('   Texture (attrib = "specular") {string {\"' + get_material (line) + '\"}}')

    elif re.search (r'^[bB]ump[mM]ap', line):
      print ('   Texture (attrib = "normal") {string {\"' + get_material (line) + '\"}}')

    elif re.search (r'^qer_editorimage', line):       
      print ('   Texture (attrib = "editor") {string {\"' + get_material (line) + '\"}}')


##########
## Main ##
##########

## Iterate over the file line by line
first_iteration = True
previous_line = ''
for current_line in fileinput.input():

  ## We have to save up 2 lines before processing
  if not first_iteration:
    process_line (previous_line, current_line)
  else:
    first_iteration = False
  previous_line = current_line

## Handle the remaining line
if previous_line:
    process_line (previous_line, '')


#######################
## replace_key_value ##
#######################

# def replace_key_value (line, key, new_key, kind, is_array):
#   global found_key_value

#   ## No need to waste time
#   if found_key_value:
#     return line

#   ## String key value needs to be wrapped in quotes
#   if not re.search (r' ' + key + ' ', line):
#     return line

#   ## We must have found one
#   found_key_value = True

#   if kind == "string":
#     text = re.sub (key + WHITE_SPACE, "   " + new_key + " {string {\"", line)
#     if text != line:
#       text = text + "}}"

#   ## Array types need an extra curly level
#   elif not is_array:
#     text = re.sub (r"\"" + key + "\" \"", "   " + new_key + " {" + kind + " {", line)
#     if text != line:
#       text = re.sub (r"\"", "}}", text.rstrip ())

#   ## Otherwise it is a normal discrete or numeric kind
#   else:
#     text = re.sub (r"\"" + key + "\" \"", "   " + new_key + " {" + kind + " {{", line)
#     if text != line:
#       text = re.sub (r"\"", "}}}", text.rstrip ())

#   ## Give the result
#   return text

