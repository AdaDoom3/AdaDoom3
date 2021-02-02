import fileinput
import re
import sys

#############
## Globals ##
#############

scope            = []
mode             = 0
in_indicies      = False
surface_done     = False
ignore_next_line = False
on_first_entity  = True
found_key_value  = False
indent           = "        "

WHITE_SPACE = r'(?:(?!\n)\s)+'
NUM_PATTERN = r'[0-9]' + WHITE_SPACE + r'[-+0-9]'
COMMA_NUM   = r'}' + WHITE_SPACE + r'{'
COMMA_NUM2  = r' \)' + WHITE_SPACE + r'\( '

###############
## Utilities ##
###############

def next_line_is_groupend (line):
    if line.strip() [0] == '}':
        return True
    return False

def do_comma_separation (line):

  ## Add comma separation between numbers
  while True:

    ## Pick out a number pair
    numbers = re.search (NUM_PATTERN, line)

    ## Numbers were found
    if numbers:
      line = line [0:numbers.start (0)] + re.sub (WHITE_SPACE, ', ', numbers.group (0)) + line [numbers.end (0):]
      numbers = False

    ## No more number pairs
    else:
      break
  return line

def replace_key_value (line, key, new_key, kind, is_array):
  global found_key_value

  ## No need to waste time
  if found_key_value:
    return line

  ## String key value needs to be wrapped in quotes
  if not re.search (r'\"' + key + '\"', line):
    return line

  ## We must have found one
  found_key_value = True

  if kind == "string":
    text = re.sub (r"\"" + key + "\" \"", "   " + new_key + " {string {\"", line)
    if text != line:
      text = text + "}}"

  ## Array types need an extra curly level
  elif not is_array:
    text = re.sub (r"\"" + key + "\" \"", "   " + new_key + " {" + kind + " {", line)
    if text != line:
      text = re.sub (r"\"", "}}", text.rstrip ())

  ## Otherwise it is a normal discrete or numeric kind
  else:
    text = re.sub (r"\"" + key + "\" \"", "   " + new_key + " {" + kind + " {{", line)
    if text != line:
      text = re.sub (r"\"", "}}}", text.rstrip ())

  ## Give the result
  return text

#################
## init_for_cm ##
#################

def init_for_cm ():
  mode = 0

#####################
## process_cm_line ##
#####################
##
## CM "1.00"
##
## 1106216053
##
## collisionModel "worldMap" {
##
##   vertices { /* numVertices = */ 32
##     /* 0 */ ( 192 -128 256 )
##     /* 1 */ ( -192 -128 256 )
##     /* 2 */ ( -192 256 256 )
##   }
##
##   edges { /* numEdges = */ 73
##     /* 0 */ ( 0 0 ) 0 0
##     /* 1 */ ( 0 1 ) 1 2
##     /* 2 */ ( 1 2 ) 1 2
##   }
##
##   nodes {
##     ( 0 128 ) 
##     ( 1 160 ) 
##     ( -1 0 ) 
##     ( -1 0 ) 
##   }
##
##   polygons /* polygonMemory = */ 2592 {
##     4 ( -62 -70 -68 -72 ) ( -1 0 0 ) 208 ( -208 -128 0 ) ( -208 256 256 ) "textures/base_wall/lfwall13f3"
##     4 ( -63 72 -67 -71 ) ( 0 1 0 ) 256 ( -208 256 0 ) ( -192 256 256 ) "textures/base_wall/lfwall13f3"
##     4 ( -64 71 -66 -69 ) ( 1 0 0 ) -192 ( -192 -128 0 ) ( -192 256 256 ) "textures/base_wall/lfwall13f3"
##   }
##
##   brushes /* brushMemory = */ 840 {
##     6 {
##       ( 0 0 -1 ) 0
##       ( 0 0 1 ) 256
##       ( 0 -1 0 ) 128
##       ( 1 0 0 ) -192
##       ( 0 1 0 ) 256
##       ( -1 0 0 ) 208
##     } ( -208 -128 0 ) ( -192 256 256 ) "solid,opaque"
##   }
## }
##

def process_cm_line (line, next_line):
  global mode

  ## Strip the EOL
  line = line.strip ()
  line = re.sub (r'//.*', '', line)

  ## Find which section we are in
  if re.search(r'vertices', line):
    mode = 1
    line = indent + "VertexArray {float[3] {"
  elif re.search(r'edges', line):
    mode = 2
    line = indent + "EdgeArray {unsigned_int32[4] {"
  elif re.search(r'nodes', line):
    mode = 3
    line = indent + "NodeArray {float[2] {"
  elif re.search(r'polygons', line):
    mode = 4
    line = indent + "PolygonArray {"
  elif re.search(r'brushes', line):
    mode = 5
    line = indent + "BrushArray {"
  else:

    ## CollisionModel header
    if re.search(r'collisionModel', line):
      line = re.sub(r'collisionModel \"', "CollisionModel $", line)
      line = re.sub(r'\" {', " {", line)

    elif mode == 0:
      if re.search(r'CM', line):
        line = ''
      elif re.search(r'[0-9]', line):
        line = 'GeometryChecksum {unsigned_int64 {' + line + '}}'

    ## Special processing for verticies
    elif mode == 1:

      ## Ending brace
      line = re.sub (r'}', '}}', line)

      ## Vertex
      if next_line_is_groupend (next_line):
        line = re.sub (r'\)', '}', line)
      else:
        line = re.sub (r'\)', '},', line)
      line = re.sub (r'\(', '{', line)
      if re.search(r'{', line):
        line = indent + line


    ## Special processing for edges
    elif mode == 2:

      ## Ending brace
      line = re.sub (r'}', '}}', line)

      ## Edge
      line = re.sub (r'\(', '{', line)
      text = line
      line = re.sub (r'\) ', '', line)
      if text != line:
        if next_line_is_groupend (next_line):
           line = line + " }"
        else:
           line = line + " },"
      if re.search(r'{', line):
        line = indent + line

    ## Special processing for nodes 
    elif mode == 3:

      ## Ending brace
      line = re.sub (r'}', '}}', line)

      ## Nodes
      if next_line_is_groupend (next_line):
        line = re.sub (r'\)', '}', line)
      else:
        line = re.sub (r'\)', '},', line)
      line = re.sub (r'\(', indent + '{', line)

    ## Special processing for polygons 
    elif mode == 4 and re.search(r'\) \(', line):
      newLine = ""
      braceCount = 0
      for C in line:
        if C == '(':
          braceCount = braceCount + 1
          if braceCount == 1:
            newLine = newLine + indent +  "Edge {int32 "
          elif braceCount == 2:
            newLine = newLine + "}\n" + indent + indent + indent + "Plane {float[4] {"
          elif braceCount == 3:
            newLine = newLine + "}}}\n" + indent + indent +indent + "VertexArray {float[3] {"
        if braceCount != 0 and not (C == ')' and braceCount == 2):
          newLine = newLine + C

      line = newLine

      ## 
      line = indent + indent + "Polygon {\n" + indent + indent + line

      ## 
      line = re.sub (r'\) \"', '}}}\n' + indent + indent + indent + 'MaterialName {string {\"', line) + '}}\n' + indent + indent + "}"

      ## Ditch the rounded braces
      line = re.sub (r'\)', '}', line)
      line = re.sub (r'\(', '{', line)

      ## Fixup textures
      line = re.sub(r'textures/', "", line)
      line = re.sub(r'/', "_", line)


    ## Special processing for brushes 
    elif mode == 5:
      if re.search(r'} \(', line):
        line = re.sub(r'} \(', "VertexArray {float [3] {{", line)
        line = indent + indent + line
        if re.search(r'playerclip', line):
          line = indent + indent + indent + 'Player {bool {true}}\n' + line
        if re.search(r'opaque', line):
          line = indent + indent + indent + 'Opaque {bool {true}}\n' + line
        if re.search(r'water', line):
          line = indent + indent + indent + 'Water {bool {true}}\n' + line
        if re.search(r'solid', line):
          line = indent + indent + indent + 'Solid {bool {true}}\n' + line
        if re.search(r'monsterclip', line):
          line = indent + indent + indent + 'Monster {bool {true}}\n' + line
        if re.search(r'moveableclip', line):
          line = indent + indent + indent + 'Moveable {bool {true}}\n' + line
        if re.search(r'aas_solid', line):
          line = indent + indent + indent + 'Bot {bool {true}}\n' + line
        if re.search(r'blood', line):
          line = indent + indent + indent + 'Blood {bool {true}}\n' + line
        if re.search(r'trigger', line):
          line = indent + indent + indent + 'Trigger {bool {true}}\n' + line
        if re.search(r'body', line):
          line = indent + indent + indent + 'DeadBody {bool {true}}\n' + line
        if re.search(r'flashlight_trigger', line):
          line = indent + indent + indent + 'Flashlight {bool {true}}\n' + line
        if re.search(r'corpse', line):
          line = indent + indent + indent + 'Corpse {bool {true}}\n' + line
        if re.search(r'ikclip', line):
          line = indent + indent + indent + 'Animation {bool {true}}\n' + line
        if re.search(r'aas_obstacle', line):
          line = indent + indent + 'Obstacle {bool {true}}\n' + line
        line = indent + indent + indent + "}\n" + line

        ## Clear the flags
        text = ''
        for C in line:
          if C == '"':
            break
          text = text + C
        line = text

        ## Ending
        line = line + "}}\n" + indent + indent + "}"

      elif re.search(r'{', line):
        line = indent + indent + 'CollisionBrush {\n' + indent + indent + indent + "float[4] {"

      elif re.search(r'\)', line):
        line = re.sub (r'\)', '', line)
        if next_line_is_groupend (next_line):
          line = indent + indent + line + "}"
        else:
          line = indent + indent + line + "},"

      ## Ditch the rounded braces
      line = re.sub (r'\)', '}', line)
      line = re.sub (r'\(', '{', line)

  ## Add comma separation between number groups on the same line
  print (re.sub (r'}(?:(?!\n)\s)*{', '},{', do_comma_separation (line)))

##################
## init_for_aas ##
##################


######################
## process_aas_line ##
######################
##
## DewmAAS "1.07"
##
## 4082973287
##
## settings
## {
##   bboxes
##   {
##     (-47 -47 0)-(47 47 96)
##   }
##   usePatches = 0
##   writeBrushMap = 0
##   playerFlood = 0
##   allowSwimReachabilities = 0
##   allowFlyReachabilities = 0
##   fileExtension = "aas96"
##   gravity = (0 0 -1050)
##   maxStepHeight = 18
##   maxBarrierHeight = 32
##   maxWaterJumpHeight = 20
##   maxFallHeight = 64
##   minFloorCos = 0.6999999881
##   tt_barrierJump = 100
##   tt_startCrouching = 100
##   tt_waterJump = 100
##   tt_startWalkOffLedge = 100
## }
## planes 1302 {
##   0 ( 1 0 0 2560 )
##   1 ( -1 0 0 -2560 )
##   2 ( 1 0 0 3072 )
##   3 ( -1 0 0 -3072 )
## }
## vertices 157 {
##   0 ( 2097 -1263 -352 )
##   1 ( 1935 -1263 -352 )
##   2 ( 2097 -1271 -352 )
## }
## edges 215 {
##   0 ( 0 0 )
##   1 ( 0 1 )
##   2 ( 0 2 )
## }
## edgeIndex 290 {
##   0 ( -1 )
##   1 ( 2 )
##   2 ( 3 )
##   3 ( 4 )
## }
## faces 451 {
##   0 ( 0 0 0 0 0 0 )
##   1 ( 1005 1 1 0 0 0 )
##   2 ( 1008 4 1 0 0 5 )
##   3 ( 581 1 1 0 0 0 )
## }
## faceIndex 573 {
##   0 ( 1 )
##   1 ( 2 )
##   2 ( 3 )
##   3 ( 4 )
##   4 ( 5 )
## }
## areas 75 {
##   0 ( 0 0 0 0 0 0 ) 0 {
##   }
##   1 ( 65 0 0 8 1 0 ) 2 {
##     2 11 (1935 -1272 -352) (1934.5 -1272 -352) 5 1
##     2 4 (2097 -1276 -352) (2097.5 -1276 -352) 3 1
##   }
##   2 ( 65 0 8 9 1 1 ) 4 {
##     2 3 (2088 -1471 -352) (2088 -1470.5 -352) 10 1
##     2 5 (2097 -1472 -352) (2097.5 -1472 -352) 11 1
##     2 20 (1981 -1471 -352) (1981 -1470.5 -352) 8 1
##     2 13 (1977 -1472 -352) (1976.5 -1472 -352) 7 1
##   }
## }
## nodes 293 {
##   0 ( 0 0 0 )
##   1 ( 0 0 2 )
##   2 ( 802 3 58 )
##   3 ( 4 0 4 )
##   4 ( 304 5 30 )
##   5 ( 306 0 6 )
##   6 ( 10 0 7 )
##   7 ( 12 8 0 )
## }
## portals 3 {
##   0 ( 0 0 0 0 0 )
##   1 ( 37 1 2 40 13 )
##   2 ( 53 1 3 39 4 )
## }
## portalIndex 4 {
##   0 ( 2 )
##   1 ( 1 )
##   2 ( 1 )
##   3 ( 2 )
## }
## clusters 4 {
##   0 ( 0 0 49730304 0 )
##   1 ( 41 41 0 2 )
##   2 ( 14 14 2 1 )
##   3 ( 5 5 3 1 )
## }



###################
## init_for_proc ##
###################

def init_for_proc ():
  mode             = 0
  in_indicies      = False
  surface_done     = False
  ignore_next_line = False

#######################
## process_proc_line ##
#######################
##
## mapProcFile003
##
## model { /* name = */ "_area0" /* numSurfaces = */ 3
##
##   /* surface 0 */ { "textures/base_wall/lfwall27d" /* numVerts = */ 4 /* numIndexes = */ 6
##     ( -192 256 256 4 3 0 0 -1 ) ( 192 -128 256 -2 -3 0 0 -1 ) ( 192 256 256 4 -3 0 0 -1 ) 
##     ( -192 -128 256 -2 3 0 0 -1 ) 
##     0 1 2 3 1 0 
##   }
##
##   /* surface 1 */ { "textures/base_wall/lfwall27b" /* numVerts = */ 4 /* numIndexes = */ 6
##     ( -192 256 0 4 -3 0 0 1 ) ( 192 256 0 4 3 0 0 1 ) ( 192 -128 0 -2 3 0 0 1 ) 
##     ( -192 -128 0 -2 -3 0 0 1 ) 
##     0 1 2 3 0 2 
##   }
##
##   /* surface 2 */ { "textures/base_wall/lfwall13f3" /* numVerts = */ 16 /* numIndexes = */ 24
##     ( -192 256 256 -1 -2 0 -1 0 ) ( 192 256 256 2 -2 0 -1 0 ) ( 192 256 0 2 0 0 -1 0 ) 
##     ( -192 256 0 -1 0 0 -1 0 ) ( -192 -128 256 2 -2 0 1 0 ) ( 192 -128 0 -1 0 0 1 0 ) 
##     ( 192 -128 256 -1 -2 0 1 0 ) ( -192 -128 0 2 0 0 1 0 ) ( 192 -128 256 1 -2 -1 0 0 ) 
##     0 1 2 3 0 2 4 5 6 7 5 4 8 9 10 11 9 8 
##   }
##
## }
##
## interAreaPortals { /* numAreas = */ 34 /* numIAP = */ 43
##
##   /* interAreaPortal format is: numPoints positiveSideArea negativeSideArea ( point) ... */
##   /* iap 0 */ 4 1 0 ( 1168 184 192 ) ( 1040 184 192 ) ( 1040 184 400 ) ( 1168 184 400 ) 
##   /* iap 1 */ 4 1 2 ( 1040 184 192 ) ( 1040 -48 192 ) ( 1040 -48 400 ) ( 1040 184 400 ) 
##   /* iap 2 */ 4 4 1 ( 1168 -208 184 ) ( 1040 -208 184 ) ( 1040 -208 328 ) ( 1168 -208 328 ) 
## }
##
## nodes { /* numNodes = */ 463
## 
##   /* node format is: ( planeVector ) positiveChild negativeChild */
##   /* a child number of 0 is an opaque, solid area */
##   /* negative child numbers are areas: (-1-child) */
##   /* node 0 */ ( 1 0 0 0 ) 1 140
##   /* node 1 */ ( 0 1 0 0 ) 2 34
##   /* node 2 */ ( -0.7071069479 0.7071066499 0 -45.2549667358 ) 6 8
## }
##
## shadowModel { /* name = */ "_prelight_nkd_light_163"
##
##   /* numVerts = */ 148 /* noCaps = */ 84 /* noFrontCaps = */ 156 /* numIndexes = */ 228 /* planeBits = */ 59
##   ( 408 1152 256 ) ( 416 1151 253 ) ( 408 1152 320 ) ( 416 1151 322 ) ( 416 1152 240 ) 
##   ( 416 1152 240 ) ( 377 1152 256 ) ( 416 1147 240 ) ( 408 1152 256 ) ( 416 1151 253 ) 
##   ( 416 1152 240 ) ( 416 1152 240 ) ( 416 1152 336 ) ( 416 1152 336 ) ( 416 1152 336 ) 
##   0 2 1 2 3 1 20 22 21 22 23 21 12 4 5 12 5 13 
## }
##

materials = {}

def process_proc_line (line, next_line):
  global mode
  global in_indicies
  global surface_done
  global ignore_next_line

  ## Ignore the next line 
  if ignore_next_line:
    ignore_next_line = False
    return

  ## Strip the EOL
  line = line.strip ()
  line = re.sub (r'//.*', '', line)

  ## Ignore empty lines
  if not line:
    return

  ## Model
  if re.search (r'model {', line):
    mode = 1

    ## Output structs
    line = re.sub (r'model { /\* name = \*/ \"', "GeometryObject $", line)
    line = re.sub (r'\" /\* numSurfaces = \*/', ' //', line)
    print (line + '\n{')

  ## Surface
  elif re.search (r'/\* surface', line):
    mode = 2

    ## Get the material and sanatize it
    line = re.search (r'\"([a-z_/A-Z0-9-])*\"', line)
    line = line.group (0)
    line = re.sub (r'/', '_', line)

    ## Output structs
    materials [line] = 1 #materials [line] + 1
    print ('   Mesh\n   {\n      MaterialName {string {' + line + '}}')
    print ('      VertexArray\n      {\n         float[8]\n         {')

    ## Reset parsing globals
    in_indicies  = False
    surface_done = False

  ## Portal
  elif re.search (r'interAreaPortals {', line):
    mode = 3
    print ('PortalArray')
    print ('{')

  ## Nodes
  elif re.search (r'nodes {', line):
    mode = 4
    line = re.sub ('nodes { /\* numNodes = \*/', 'NodeArray //', line)
    line = line + '\n{'

    print (line)

  ## Shadow model
  elif re.search (r'shadowModel {', line):
    mode = 5

    ## Output structs
    line = re.sub (r'shadowModel { /\* name = \*/ \"', 'ShadowObject $', line)
    line = re.sub (r'\"', '\n{', line)
    print (line)

  ## General processing
  else:

    ## Surface processing
    if mode == 1:
      print (line)

    elif mode == 2:

      ## Ignore trailing crap
      if surface_done:
        print (line)
        return

      ## Add comma separation for verticies
      if not in_indicies:
        line = '            ' + line
        line = re.sub (COMMA_NUM2, '}, {', line)
        line = re.sub (r'\( ', '{', line)
        line = re.sub (r' \)', '}', line)

      ## Check if we are the last line and output accordingly
      if not re.search (r'\(', next_line) and not in_indicies:
        in_indicies = True
        line = line + '\n         }\n      }\n      IndexArray\n      {\n         unsigned_int32\n         {'
      elif in_indicies:
        line = '            ' + line
        if re.search (r'}', next_line):
          line = line + '\n         }\n      }\n   }'
          surface_done = True
          ignore_next_line = True
        else:
          line = line + ','
      else:
        line = line + ','

      ## Output surface
      print (do_comma_separation (line))

    ## Portal processing
    elif mode == 3:

      ## We only care if we are looking at data
      if not re.search (r'iap', line):
        return

      ## Layout portal decl and attributes
      line = re.sub (r'/\* iap ', '   Portal $portal', line)
      line = re.sub (r' \*/ ', '\n   {\n      unsigned_int[3] {{', line)

      ## Make the vertex array
      rounded_braces = re.search (r' \(', line)
      line = line [:rounded_braces.start (0)] + '}}\n      VertexArray\n      {\n         float[3] {' + line [rounded_braces.start (0):] + '}'
      line = line + '\n      }\n   }'

      ## Clean up commas
      line = re.sub (COMMA_NUM2, '}, {', line)
      line = re.sub (r' \( ', '{', line)
      line = re.sub (r' \)', '}', line)

      ## Output portal
      print (do_comma_separation (line))

      ## Output the closing brace
      if re.search (r'}', next_line):
        print ('}')

    ## Shadow processing
    elif mode == 5:

      ## First line with info
      if re.search (r'/\*', line):
        line = re.sub (r'/\* numVerts = \*/ ', '   VertexCount {int32 {', line)
        line = re.sub (r' /\* noCaps = \*/ ', '}}\n   NoCaps {int32 {', line)
        line = re.sub (r' /\* noFrontCaps = \*/ ', '}}\n   NoFrontCaps {int32 {', line)
        line = re.sub (r' /\* numIndexes = \*/ ', '}}\n   IndexCount {int32 {', line)
        line = re.sub (r' /\* planeBits = \*/ ', '}}\n   PlaneBits {unsigned_int32 {', line)
        line = line + '}}\n   VertexArray\n   {\n      float[3]\n      {'
        print (line)

      ## Verticies
      elif re.search (r'\(', line):
        line = '         ' + line
        line = re.sub (COMMA_NUM2, '}, {', line)
        line = re.sub (r'\( ', '{', line)
        line = re.sub (r' \)', '}', line)
        if re.search (r'\(', next_line):
          line = line + ','
        else:
          line = line + '\n      }\n   }\n   IndexArray\n   {\n      unsigned_int32 {'
        print (do_comma_separation (line))

      ## Closing brace
      elif re.search (r'}', line):
        print ('   }\n}')

      ## Indicies
      else:
        line = '         ' + line
        if not re.search (r'}', next_line):
          line = line + ','
        else:
          line = line + '\n      }'
        print (do_comma_separation (line))

    ## Node processing
    elif mode == 4:
      if re.search (r'/\* node', line) and not re.search (r'format', line):
        line = re.sub (r'/\* node ', '   Node $node', line)
        line = re.sub (r' \*/ ', '\n   {\n      Plane {float[4] {', line)
        line = re.sub (r' \) ', '}}}\n      ChildAreas {int32[2] {{', line)
        line = re.sub (r'\( ', '{', line)
        line = line + '}}}\n   }'
        if re.search (r'}', next_line):
          line = line + '\n}'
        print (do_comma_separation (line))

##################
## init_for_map ##
##################

def init_for_map ():
  scope           = []
  mode            = 0
  on_first_entity = True
  found_key_value = False

######################
## process_map_line ##
######################
##
## Version 2
## // entity 0
## {
##   "classname" "worldspawn"
##   "name" "info_locationseparator_4"
##   "origin" "372 704 204"
##
##   // brush 0
##   {
##     brushDef3
##     {
##       ( -0 0 1 -64 ) ( ( 0.03125 -0 -0 ) ( 0 0.03125 0 ) ) "textures/common/nodraw" 0 0 0
##       ( 0 1 0 -64 ) ( ( 0.03125 0 0 ) ( 0 0.03125 0 ) ) "textures/common/nodraw" 0 0 0
##       ( 1 -0 0 -64 ) ( ( 0.03125 -0 -0 ) ( 0 0.03125 0 ) ) "textures/common/nodraw" 0 0 0
##       ( 0 0 -1 -64 ) ( ( 0.03125 -0 -0 ) ( 0 0.03125 0 ) ) "textures/common/nodraw" 0 0 0
##       ( 0 -1 0 -64 ) ( ( 0.03125 -0 -0 ) ( 0 0.03125 0 ) ) "textures/common/nodraw" 0 0 0
##       ( -1 0 0 -64 ) ( ( 0.03125 -0 -0 ) ( 0 0.03125 0 ) ) "textures/common/nodraw" 0 0 0
##     }
##   }
##
##   // primitive 0
##   {
##     patchDef2
##     {
##       "textures/common/nodraw"
##       ( 3 3 0 0 0 )
##       (
##         ( ( -64 -64 -256 0 0 ) ( -64 -64 -192 0 -2 ) ( -64 -64 -128 0 -4 ) )
##         ( ( 64 -64 -256 4 0 ) ( 64 -64 -192 4 -2 ) ( 64 -64 -128 4 -4 ) )
##         ( ( 64 64 -256 8 0 ) ( 64 64 -192 8 -2 ) ( 64 64 -128 8 -4 ) )
##       )
##     }
##   }
##
##   // primitive 1
##   {
##     patchDef3
##     {
##       "textures/decals/cam_base"
##       ( 3 3 1 1 0 0 0 )
##       (
##         ( ( -1010 576.25 101 0 0 ) ( -1012 578.25 101 0 0.5 ) ( -1014 576.25 101 0 1 ) )
##         ( ( -1010 576.25 99 0.5 0 ) ( -1012 578.25 99 0.5 0.5 ) ( -1014 576.25 99 0.5 1 ) )
##         ( ( -1010 576.25 97 1 0 ) ( -1012 578.25 97 1 0.5 ) ( -1014 576.25 97 1 1 ) )
##       )
##     }
##   }
## }
##

models = {}

def process_map_line (line, next_line):
  global mode
  global on_first_entity
  global found_key_value
  global scope

  ## Strip the EOL
  line = line.rstrip ()

  ## Fixup scope
  if re.search (r'{', line):
    scope.append ('Crap')

  if re.search (r'}', line):
    scope.pop ()
    if len (scope) == 0:
      mode = 0
    #  in_material = False
    #  print ('}')

  ## Find which section we are in
  if re.search (r'// brush', line):
    mode = 1
  elif re.search (r'brushDef3', line):
    mode = 2
  elif re.search (r'// primitive', line):
    mode = 3
  elif re.search (r'patchDef2', line):
    mode = 4
  elif re.search (r'patchDef3', line):
    mode = 5
  elif re.search (r'// entity', line):
    mode = 6
    line = re.sub (r'// entity ', "Entity $entity", line)
    if on_first_entity:
      print (line)
      on_first_entity = False
    else:
      print ("}\n" + line)
  elif mode == 0 and len (scope) == 1:
    mode = 6
    line = "Entity\n{"
    if on_first_entity:
      print (line)
      on_first_entity = False
    else:
      print ("}\n" + line)

  else:

    ## We don't care about non-entities
    if mode != 6 or re.search (r'}', line):
      return

    ## Collect the scene models so we can know the truth
    if re.search (r'\"' + "model" + '\"', line):
      x = re.sub (r"\"" + "model" + "\" \"", "", line)
      models [x] = 1
      
    ## Modify key values
    found_key_value = False
    line = replace_key_value (line, "name",                     "Name",        "string",   False)
    line = replace_key_value (line, "classname",                "Class",       "string",   False)
    line = replace_key_value (line, "model",                    "MeshName",    "string",   False) 
    line = replace_key_value (line, "origin",                   "Origin",      "float[3]", True)
    line = replace_key_value (line, "light_center",             "LightCenter", "float[3]", True)
    line = replace_key_value (line, "light_radius",             "LightRadius", "float[3]", True)
    line = replace_key_value (line, "editor_drLastCameraAngle", "LastCamAng",  "float[3]", True)
    line = replace_key_value (line, "editor_drLastCameraPos",   "LastCamPos",  "float[3]", True)

    ## Comment out unknown attributes
    if line [0] == '\"':
      line = "   // Uknown attribute: " + line

    ## Dump the result to stdout
    print (do_comma_separation (line))

##########
## Main ##
##########

file_formats = ('map', 'proc') #, 'cm')

def process_line (line, next_line):
  if f_format == 'map':
    process_map_line (line, next_line)
  elif f_format == 'proc':
    process_proc_line (line, next_line)
  elif f_format == 'cm':
    process_cm_line (line, next_line)

for f_format in file_formats:

  ## Initialize needed globals
  if f_format == 'map':
    init_for_map
  elif f_format == 'proc':
    init_for_proc
  elif f_format == 'cm':
    init_for_cm

  ## Iterate over the file line by line
  with open (sys.argv[1] + '.' + f_format) as f:
    first_iteration = True
    previous_line = ''
    for current_line in f.readlines():

      ## We have to save up 2 lines before processing
      if not first_iteration:
        process_line (previous_line, current_line)
      else:
        first_iteration = False
      previous_line = current_line

    ## Handle the remaining line
    if previous_line:
      process_line (previous_line, '')

    ## We have to append a curly when we have encountered at least one entity in a map file
    if not on_first_entity:
      print ('}')
      on_first_entity = True

## Dump materials
print ("/*")
for mat in materials.keys ():
  print (mat)
print ("*/")

## Dump models
print ("/*")
for modl in models.keys ():
  print (modl)
print ("*/")