separate(Neo.File.Model) package body Id_Software is
    function To_Internal(Data : in Parser) return Record_Map is
      Map : Record_Map := (others => <>);
      Number_Of_Worlds : Integer_4_Signed := 1;
      begin
        Data.Assert_Next("Version");
        Entity.Version := Integer'wide_value(Data.Next);
        while not Is_End(Data) loop 
          Data.Assert_Next("{");
          Token := Data.Next;
          while Token /= "}" loop
            if Token /= "{" then
              Token := Trim(Token, """", Both);
              if Token = "origin" then
                Origin := (Float_4_Real'wide_value(Data.Next), Float_4_Real'wide_value(Data.Next), Float_4_Real'wide_value(Data.Next));
              elsif Token = "classname" then
                Token := Data.Next;
                if Token = "worldspawn" then
                  Origin := (others => 0.0);
                  Entity.Values.Add("classname", "worldspawn"); 
                else
                  Entity.Values.Add("classname", Token); 
                end if;
              else
                Entity.Values.Add(Token, Data.Next);
              end if;
            else
              Token := Data.Next;
              if Token = "brush" then
                Map.Brushes.Add;
                Data.Assert_Next("{");
                Token := Data.Next;
                while Token /= "}" loop
                  while Token /= "(" loop
                    Map.Brushes.Last.Values.Add(Token, Data.Next);
                    Token := Data.Next;
                  end loop;
                  Token := Data.Next;
                  Map.Brushes.Last.Sides.Add((
                    Plane    => (if Token = "brushDef2" or Token = "brushDef3" then Data.Next_Vector(4)
                                 else Plane_From_Points(Data.Next_Vector(3), Data.Next_Vector(3), Data.Next_Vector(3), Origin)),
                    Texture  => Data.Next_Matrix(2, 3),
                    Origin   => Origin,
                    Material => (if Version < 2.0 then "textures/" else "") & Data.Next_File_Name));
                  Data.Skip_Line; -- Quake II allows override of default flags and values, but it is not needed anymore so skip it
                  Token := Data.Next;
                end loop;
              elsif Token = "patch" then -- "These were written out in the wrong order, IMHO"
                Token := Data.Next_Path;
                Map.Patches.Add(new Record_Patch(Data.Next = "patchDef3", Data.Next, Data.Next));
                Map.Patches.Last.Material := (if Version < 2.0 then "textures/" else "") & Token;
                Data.Assert_Next("(");
                if Map.Patches.Last.Is_Explicityly_Subdivided then
                  Map.Patches.Last.Subdivisions := (Data.Next, Data.Next);
                end if;
                Map.Patches.Last.Random_Information := Data.Next_Array_Integer(3);
                Data.Assert_Next((")", "("));
                for Vertex_Group of Map.Patches.Last.Vertices loop -- "vert = &((*patch)[i * patch->GetWidth + j]);"
                  Data.Assert_Next("(");
                  for Vertex of Vertex_Group loop
                    Data.Assert_Next("(");
                    Vertex := (Location => Data.Next_Array_Float(3) - Origin, Texture => Data.Next_Array_Float(2));
                    Data.Assert_Next(")");
                  end loop;
                  Data.Assert_Next(")");
                end loop;
                Token := Data.Next;
                while Token /= "}" loop
                  Map.Patches.Last.Values.Add(Token, Data.Next);
                  Token := Token.Next;
                end loop;
                Data.Assert_Next("}");
              else -- Assume it is a Quake III brush
                Map.Brushes.Add;
                Token := Data.Next;
                while Token /= "}" loop
                  Map.Brushes.Last.Sides.Add((
                    Plane    => Plane_From_Points(Data.Next_Vector(3), Data.Next_Vector(3), Data.Next_Vector(3), Origin),
                    Material => "textures/" & Data.Next,
                    Texture  => ((0.03125, 0.0, 0.0), (0.0, 0.03125, 0.0)),
                    Origin   => Origin));
                  Data.Skip(5); -- Apparently shift, rotate, and scale are ignored
                  Data.Skip_Line;
                end loop;
              end if;
            end if;
            Token := Data.Next;
          end loop;
        end loop;
        for Entity of Map.Entities loop
          for Primitive of Entity.Primitives loop
            for Side of Primitive.Sides loop
              Map.Geometry_Checksum :=
                Map.Geometry_Checksum xor 
                Side.Plane(1) xor
                Side.Plane(2) xor
                Side.Plane(3);
            end loop;
            Map.Geometry_Checksum := Map.Geometry_Checksum xor Side.Material;
          end loop;
          if Entity.Values.Element("classname") = "worldspawn" then
            --Number_Of_Worlds > 1;
            Number_Of_Worlds := Number_Of_Worlds + 1;
            if Entity.Values.Has_Element("removeEntities") then
              while
                const idKeyValue *removeEntities = entities[0]->epairs.MatchPrefix("removeEntities", NULL);
              end loop;
            end if;
            if Entity.Values.Has_Element("overrideMaterial") then
              for Overriden_Entity of Map.Entities loop
                for Primitive of Overriden_Entity loop
                  case Primitive.Kind is
                    when Brush_Primitive => for Side of Primitive.Sides loop Side.Material := Entity.Values.Element("overrideMaterial"); end loop;
                    when Patch_Primitive => Primitive.Material := Entity.Values.Element("overrideMaterial");
                  end case;
                end loop;
              end loop;
            end if;
            if Entity.Values.Has_Element("moveFuncGroups") then
              for Overriden_Entity of Map.Entities loop
                if Overriden.Entity.Element("classname") = "func_group" then
                  Entity.Primitives.Add(Overriden_Entity.Primitives);
                  Overriden_Entity.Primitives.Delete;
                end if;
              end loop;
            end if;
          end if;
        end loop;
        idTimer timer;
        timer.Start;
        if (!LoadCollisionModelFile(mapFile->GetName, mapFile->GetGeometryCRC) 
          if (!mapFile->GetNumEntities 
            return;
          }
          -- load the .proc file bsp for data optimisation
          LoadProcBSP(mapFile->GetName);
          -- load it
          idStrStatic< MAX_OSPATH > fileName = name;
          -- check for generated file
          idStrStatic< MAX_OSPATH > generatedFileName = fileName;
          generatedFileName.Insert("generated/", 0);
          generatedFileName.SetFileExtension(CM_BINARYFILE_EXT);
          -- if we are reloading the same map, check the timestamp
          -- and try to skip all the work
          ID_TIME_T currentTimeStamp = fileSystem->GetTimestamp(fileName);
          -- see if we have a generated version of this 
          bool loaded = false;
          idFileLocal file(fileSystem->OpenFileReadMemory(generatedFileName));
          if (file != NULL 
            int numEntries = 0;
            file->ReadBig(numEntries);
            file->ReadString(mapName);
            file->ReadBig(crc);
            idStrStatic< 32 > fileID;
            idStrStatic< 32 > fileVersion;
            file->ReadString(fileID);
            file->ReadString(fileVersion);
            if (fileID == CM_FILEID && fileVersion == CM_FILEVERSION && crc == mapFileCRC && numEntries > 0 
              for (int i = 0; i < numEntries; i++ 
                cm_model_t *model = LoadBinaryModelFromFile(file, currentTimeStamp);
                models[ numModels ] = model;
                numModels++;
              }
              loaded = true;
            }
          }
          if (!loaded 
            fileName.SetFileExtension(CM_FILE_EXT);
            src = new (TAG_COLLISION) idLexer(fileName);
            src->SetFlags(LEXFL_NOSTRINGCONCAT | LEXFL_NODOLLARPRECOMPILE);
            if (!src->IsLoaded 
              delete src;
              return false;
            }
            int numEntries = 0;
            idFileLocal outputFile(fileSystem->OpenFileWrite(generatedFileName, "fs_basepath"));
            if (outputFile != NULL 
              outputFile->WriteBig(numEntries);
              outputFile->WriteString(mapName);
              outputFile->WriteBig(mapFileCRC);
              outputFile->WriteString(CM_FILEID);
              outputFile->WriteString(CM_FILEVERSION);
            }
            if (!src->ExpectTokenString(CM_FILEID) 
              common->Warning("%s is not an CM file.", fileName.c_str);
              delete src;
              return false;
            }
            if (!src->ReadToken(&token) || token != CM_FILEVERSION 
              common->Warning("%s has version %s instead of %s", fileName.c_str, token.c_str, CM_FILEVERSION);
              delete src;
              return false;
            }
            if (!src->ExpectTokenType(TT_NUMBER, TT_INTEGER, &token) 
              common->Warning("%s has no map file CRC", fileName.c_str);
              delete src;
              return false;
            }
            crc = token.GetUnsignedLongValue;
            if (mapFileCRC && crc != mapFileCRC 
              common->Printf("%s is out of date\n", fileName.c_str);
              delete src;
              return false;
            }
            -- parse the file
            while (1 
              if (!src->ReadToken(&token) 
                break;
              }
              if (token == "collisionModel" 
                cm_model_t *model;
                idToken token;

                if (numModels >= MAX_SUBMODELS 
                  common->Error("LoadModel: no free slots");
                  return NULL;
                }
                model = AllocModel;
                models[numModels ] = model;
                numModels++;
                -- parse the file
                src->ExpectTokenType(TT_STRING, 0, &token);
                model->name = token;
                src->ExpectTokenString("{");
                while (!src->CheckTokenString("}") 
                  src->ReadToken(&token);
                  if (token == "vertices" 
                    ParseVertices(src, model);
                    src->ExpectTokenString("{");
                    model->numVertices = src->ParseInt;
                    model->maxVertices = model->numVertices;
                    model->vertices = (cm_vertex_t *) Mem_ClearedAlloc(model->maxVertices * sizeof(cm_vertex_t), TAG_COLLISION);
                    for (i = 0; i < model->numVertices; i++ 
                      src->Parse1DMatrix(3, model->vertices[i].p.ToFloatPtr);
                      model->vertices[i].side = 0;
                      model->vertices[i].sideSet = 0;
                      model->vertices[i].checkcount = 0;
                    }
                    src->ExpectTokenString("}");
                    continue;
                  }
                  if (token == "edges" 
                    ParseEdges(src, model);
                    src->ExpectTokenString("{");
                    model->numEdges = src->ParseInt;
                    model->maxEdges = model->numEdges;
                    model->edges = (cm_edge_t *) Mem_ClearedAlloc(model->maxEdges * sizeof(cm_edge_t), TAG_COLLISION);
                    for (i = 0; i < model->numEdges; i++ 
                      src->ExpectTokenString("(");
                      model->edges[i].vertexNum[0] = src->ParseInt;
                      model->edges[i].vertexNum[1] = src->ParseInt;
                      src->ExpectTokenString(")");
                      model->edges[i].side = 0;
                      model->edges[i].sideSet = 0;
                      model->edges[i].internal = src->ParseInt;
                      model->edges[i].numUsers = src->ParseInt;
                      model->edges[i].normal = vec3_origin;
                      model->edges[i].checkcount = 0;
                      model->numInternalEdges += model->edges[i].internal;
                    }
                    src->ExpectTokenString("}");
                    continue;
                  }
                  if (token == "nodes" 
                    src->ExpectTokenString("{");
                    model->node = ParseNodes(src, model, NULL);
                    cm_node_t *node;
                    model->numNodes++;
                    node = AllocNode(model, model->numNodes < NODE_BLOCK_SIZE_SMALL ? NODE_BLOCK_SIZE_SMALL : NODE_BLOCK_SIZE_LARGE);
                    node->brushes = NULL;
                    node->polygons = NULL;
                    node->parent = parent;
                    src->ExpectTokenString("(");
                    node->planeType = src->ParseInt;
                    node->planeDist = src->ParseFloat;
                    src->ExpectTokenString(")");
                    if (node->planeType != -1 
                      node->children[0] = ParseNodes(src, model, node);
                      node->children[1] = ParseNodes(src, model, node);
                    }
                    return node;
                    src->ExpectTokenString("}");
                    continue;
                  }
                  if (token == "polygons" 
                    cm_polygon_t *p;
                    int i, numEdges;
                    idVec3 normal;
                    idToken token;
                    if (src->CheckTokenType(TT_NUMBER, 0, &token) 
                      model->polygonBlock = (cm_polygonBlock_t *) Mem_ClearedAlloc(sizeof(cm_polygonBlock_t) + token.GetIntValue, TAG_COLLISION);
                      model->polygonBlock->bytesRemaining = token.GetIntValue;
                      model->polygonBlock->next = ((byte *) model->polygonBlock) + sizeof(cm_polygonBlock_t);
                    }
                    src->ExpectTokenString("{");
                    while (!src->CheckTokenString("}") 
                      -- parse polygon
                      numEdges = src->ParseInt;
                      p = AllocPolygon(model, numEdges);
                      p->numEdges = numEdges;
                      src->ExpectTokenString("(");
                      for (i = 0; i < p->numEdges; i++ 
                        p->edges[i] = src->ParseInt;
                      }
                      src->ExpectTokenString(")");
                      src->Parse1DMatrix(3, normal.ToFloatPtr);
                      p->plane.SetNormal(normal);
                      p->plane.SetDist(src->ParseFloat);
                      src->Parse1DMatrix(3, p->bounds[0].ToFloatPtr);
                      src->Parse1DMatrix(3, p->bounds[1].ToFloatPtr);
                      src->ExpectTokenType(TT_STRING, 0, &token);
                      -- get material
                      p->material = declManager->FindMaterial(token);
                      p->contents = p->material->GetContentFlags;
                      p->checkcount = 0;
                      -- filter polygon into tree
                      R_FilterPolygonIntoTree(model, model->node, NULL, p);
                    }
                    continue;
                  }
                  if (token == "brushes" 
                    cm_brush_t *b;
                    int i, numPlanes;
                    idVec3 normal;
                    idToken token;
                    if (src->CheckTokenType(TT_NUMBER, 0, &token) 
                      model->brushBlock = (cm_brushBlock_t *) Mem_ClearedAlloc(sizeof(cm_brushBlock_t) + token.GetIntValue, TAG_COLLISION);
                      model->brushBlock->bytesRemaining = token.GetIntValue;
                      model->brushBlock->next = ((byte *) model->brushBlock) + sizeof(cm_brushBlock_t);
                    }
                    src->ExpectTokenString("{");
                    while (!src->CheckTokenString("}") 
                      -- parse brush
                      numPlanes = src->ParseInt;
                      b = AllocBrush(model, numPlanes);
                      b->numPlanes = numPlanes;
                      src->ExpectTokenString("{");
                      for (i = 0; i < b->numPlanes; i++ 
                        src->Parse1DMatrix(3, normal.ToFloatPtr);
                        b->planes[i].SetNormal(normal);
                        b->planes[i].SetDist(src->ParseFloat);
                      }
                      src->ExpectTokenString("}");
                      src->Parse1DMatrix(3, b->bounds[0].ToFloatPtr);
                      src->Parse1DMatrix(3, b->bounds[1].ToFloatPtr);
                      src->ReadToken(&token);
                      if (token.type == TT_NUMBER 
                        b->contents = token.GetIntValue;    -- old .cm files use a single integer
                      } else {
                        b->contents = ContentsFromString(token);
                      }
                      b->checkcount = 0;
                      b->primitiveNum = 0;
                      b->material = NULL;
                      -- filter brush into tree
                      R_FilterBrushIntoTree(model, model->node, NULL, b);
                    }
                    continue;
                  }
                  src->Error("ParseCollisionModel: bad token \"%s\"", token.c_str);
                }
                -- calculate edge normals
                checkCount++;
                CalculateEdgeNormals(model, model->node);
                -- get model bounds from brush and polygon bounds
                CM_GetNodeBounds(&model->bounds, model->node);
                -- get model contents
                model->contents = CM_GetNodeContents(model->node);
                -- total memory used by this model
                model->usedMemory = model->numVertices * sizeof(cm_vertex_t) +
                          model->numEdges * sizeof(cm_edge_t) +
                          model->polygonMemory +
                          model->brushMemory +
                          model->numNodes * sizeof(cm_node_t) +
                          model->numPolygonRefs * sizeof(cm_polygonRef_t) +
                          model->numBrushRefs * sizeof(cm_brushRef_t);
                return model;
                cm_model_t *model = ParseCollisionModel(src);
                if (model == NULL 
                  delete src;
                  return false;
                }
                if (outputFile != NULL 
                  WriteBinaryModelToFile(model, outputFile, currentTimeStamp);
                  numEntries++;
                }
                continue;
              }
              src->Error("idCollisionModelManagerLocal::LoadCollisionModelFile: bad token \"%s\"", token.c_str);
            }
            delete src;
            if (outputFile != NULL 
              outputFile->Seek(0, FS_SEEK_SET);
              outputFile->WriteBig(numEntries);
            }
          }
          return true;
          -- load it
          filename = name;
          filename.SetFileExtension(PROC_FILE_EXT);
          src = new (TAG_COLLISION) idLexer(filename, LEXFL_NOSTRINGCONCAT | LEXFL_NODOLLARPRECOMPILE);
          if (!src->IsLoaded 
            common->Warning("idCollisionModelManagerLocal::LoadProcBSP: couldn't load %s", filename.c_str);
            delete src;
            return;
          }
          if (!src->ReadToken(&token) || token.Icmp(PROC_FILE_ID) 
            common->Warning("idCollisionModelManagerLocal::LoadProcBSP: bad id '%s' instead of '%s'", token.c_str, PROC_FILE_ID);
            delete src;
            return;
          }
          -- parse the file
          while (1 
            if (!src->ReadToken(&token) 
              break;
            }
            if (token == "model" | == "shadowModel" | token == "interAreaPortals" 
              idToken token;
              int depth;

              depth = parseFirstBrace ? 0 : 1;
              do {
                if (!ReadToken(&token) 
                  return false;
                }
                if (token.type == TT_PUNCTUATION 
                  if (token == "{" 
                    depth++;
                  } else if (token == "}" 
                    depth--;
                  }
                }
              } while(depth);
              continue;
            }
            if (token == "nodes" 
              src->ExpectTokenString("{");
              numProcNodes = src->ParseInt;
              if (numProcNodes < 0 
                src->Error("ParseProcNodes: bad numProcNodes");
              }
              procNodes = (cm_procNode_t *)Mem_ClearedAlloc(numProcNodes * sizeof(cm_procNode_t), TAG_COLLISION);
              for (i = 0; i < numProcNodes; i++ 
                cm_procNode_t *node;
                node = &procNodes[i];
                src->Parse1DMatrix(4, node->plane.ToFloatPtr);
                if (!idLexer::ExpectTokenString("(") 
                  return false;
                }

                for (i = 0; i < x; i++ 
                  m[i] = idLexer::ParseFloat;
                }

                if (!idLexer::ExpectTokenString(")") 
                  return false;
                }
                return true;
                node->children[0] = src->ParseInt;
                node->children[1] = src->ParseInt;
              }
              src->ExpectTokenString("}");
              break;
            }
            src->Error("idCollisionModelManagerLocal::LoadProcBSP: bad token \"%s\"", token.c_str);
          }
          -- convert brushes and patches to collision data
          for (i = 0; i < mapFile->GetNumEntities; i++ 
            mapEnt = mapFile->GetEntity(i);
            if (numModels >= MAX_SUBMODELS 
              common->Error("idCollisionModelManagerLocal::BuildModels: more than %d collision models", MAX_SUBMODELS);
              break;
            }
            models[numModels] = CollisionModelForMapEntity(mapEnt);
            if (models[ numModels] 
              numModels++;
            }
          }
          -- free the proc bsp which is only used for data optimization
          Mem_Free(procNodes);
          procNodes = NULL;
          -- write the collision models to a file
          WriteCollisionModelsToFile(mapFile->GetName, 0, numModels, mapFile->GetGeometryCRC);
        } 
        idLexer src(LEXFL_NOFATALERRORS | LEXFL_NOSTRINGESCAPECHARS | LEXFL_NOSTRINGCONCAT | LEXFL_ALLOWPATHNAMES);
        idToken token;
        int depth;
        unsigned int c;
        name = fileName;
        crc = mapFileCRC;
        common->Printf("[Load AAS]\n");
        common->Printf("loading %s\n", name.c_str);
        if (!src.LoadFile(name) 
          return false;
        }
        if (!src.ExpectTokenString(AAS_FILEID) 
          common->Warning("Not an AAS file: '%s'", name.c_str);
          return false;
        }
        if (!src.ReadToken(&token) || token != AAS_FILEVERSION 
          common->Warning("AAS file '%s' has version %s instead of %s", name.c_str, token.c_str, AAS_FILEVERSION);
          return false;
        }
        if (!src.ExpectTokenType(TT_NUMBER, TT_INTEGER, &token) 
          common->Warning("AAS file '%s' has no map file CRC", name.c_str);
          return false;
        }
        c = token.GetUnsignedLongValue;
        if (mapFileCRC && c != mapFileCRC 
          common->Warning("AAS file '%s' is out of date", name.c_str);
          return false;
        }
        -- clear the file in memory
        Clear;
        -- parse the file
        while (1 
          if (!src.ReadToken(&token) 
            break;
          }
          if (token == "settings" 
            if (!settings.FromParser(src)  return false; }
          }
          else if (token == "planes" 
            int numPlanes, i;
            idPlane plane;
            idVec4 vec;

            numPlanes = src.ParseInt;
            planeList.Resize(numPlanes);
            if (!src.ExpectTokenString("{") 
              return false;
            }
            for (i = 0; i < numPlanes; i++ 
              src.ParseInt;
              if (!src.Parse1DMatrix(4, vec.ToFloatPtr) 
                return false;
              }
              plane.SetNormal(vec.ToVec3);
              plane.SetDist(vec[3]);
              planeList.Append(plane);
            }
            if (!src.ExpectTokenString("}") 
              return false;
            }
            return true;
          }
          else if (token == "vertices" 
            int numVertices, i;
            idVec3 vec;

            numVertices = src.ParseInt;
            vertices.Resize(numVertices);
            if (!src.ExpectTokenString("{") 
              return false;
            }
            for (i = 0; i < numVertices; i++ 
              src.ParseInt;
              if (!src.Parse1DMatrix(3, vec.ToFloatPtr) 
                return false;
              }
              vertices.Append(vec);
            }
            if (!src.ExpectTokenString("}") 
              return false;
            }
            return true;
          }
          else if (token == "edges" 
            int numEdges, i;
            aasEdge_t edge;
            numEdges = src.ParseInt;
            edges.Resize(numEdges);
            if (!src.ExpectTokenString("{") 
              return false;
            }
            for (i = 0; i < numEdges; i++ 
              src.ParseInt;
              src.ExpectTokenString("(");
              edge.vertexNum[0] = src.ParseInt;
              edge.vertexNum[1] = src.ParseInt;
              src.ExpectTokenString(")");
              edges.Append(edge);
            }
            if (!src.ExpectTokenString("}") 
              return false;
            }
            return true;
          }
          else if (token == "edgeIndex" 
              if (!ParseIndex(src, edgeIndex)  return false; }
            }
            else if (token == "faceIndex" 
              if (!ParseIndex(src, faceIndex)  return false; }
            }
            else if (token == "portalIndex" 
              if (!ParseIndex(src, portalIndex)  return false; }
            }
            int numIndexes, i;
            aasIndex_t index;

            numIndexes = src.ParseInt;
            indexes.Resize(numIndexes);
            if (!src.ExpectTokenString("{") 
              return false;
            }
            for (i = 0; i < numIndexes; i++ 
              src.ParseInt;
              src.ExpectTokenString("(");
              index = src.ParseInt;
              src.ExpectTokenString(")");
              indexes.Append(index);
            }
            if (!src.ExpectTokenString("}") 
              return false;
            }
            return true;
          else if (token == "faces" 
            int numFaces, i;
            aasFace_t face;
            numFaces = src.ParseInt;
            faces.Resize(numFaces);
            if (!src.ExpectTokenString("{") 
              return false;
            }
            for (i = 0; i < numFaces; i++ 
              src.ParseInt;
              src.ExpectTokenString("(");
              face.planeNum = src.ParseInt;
              face.flags = src.ParseInt;
              face.areas[0] = src.ParseInt;
              face.areas[1] = src.ParseInt;
              face.firstEdge = src.ParseInt;
              face.numEdges = src.ParseInt;
              src.ExpectTokenString(")");
              faces.Append(face);
            }
            if (!src.ExpectTokenString("}") 
              return false;
            }
            return true;
          }
          else if (token == "areas" 
            int numAreas, i;
            aasArea_t area;

            numAreas = src.ParseInt;
            areas.Resize(numAreas);
            if (!src.ExpectTokenString("{") 
              return false;
            }
            for (i = 0; i < numAreas; i++ 
              src.ParseInt;
              src.ExpectTokenString("(");
              area.flags = src.ParseInt;
              area.contents = src.ParseInt;
              area.firstFace = src.ParseInt;
              area.numFaces = src.ParseInt;
              area.cluster = src.ParseInt;
              area.clusterAreaNum = src.ParseInt;
              src.ExpectTokenString(")");
              areas.Append(area);
              ParseReachabilities(src, i);
              int num, j;
              aasArea_t *area;
              idReachability reach, *newReach;
              idReachability_Special *special;
              area = &areas[areaNum];
              num = src.ParseInt;
              src.ExpectTokenString("{");
              area->reach = NULL;
              area->rev_reach = NULL;
              area->travelFlags = AreaContentsTravelFlags(areaNum);
              for (j = 0; j < num; j++ 
                Reachability_Read(src, &reach);
                switch(reach.travelType 
                  case TFL_SPECIAL:
                    newReach = special = new (TAG_AAS) idReachability_Special;
                    Reachability_Special_Read(src, special);
                    break;
                  default:
                    newReach = new (TAG_AAS) idReachability;
                    break;
                }
                newReach->CopyBase(reach);
                newReach->fromAreaNum = areaNum;
                newReach->next = area->reach;
                area->reach = newReach;
              }
              src.ExpectTokenString("}");
              return true;
            }
            if (!src.ExpectTokenString("}") 
              return false;
            }
            int i;
            idReachability *reach;
            -- link reversed reachabilities
            for (i = 0; i < areas.Num; i++ 
              for (reach = areas[i].reach; reach; reach = reach->next 
                reach->rev_next = areas[reach->toAreaNum].rev_reach;
                areas[reach->toAreaNum].rev_reach = reach;
              }
            }
            return true;
          }
          else if (token == "nodes" 
            int numNodes, i;
            aasNode_t node;

            numNodes = src.ParseInt;
            nodes.Resize(numNodes);
            if (!src.ExpectTokenString("{") 
              return false;
            }
            for (i = 0; i < numNodes; i++ 
              src.ParseInt;
              src.ExpectTokenString("(");
              node.planeNum = src.ParseInt;
              node.children[0] = src.ParseInt;
              node.children[1] = src.ParseInt;
              src.ExpectTokenString(")");
              nodes.Append(node);
            }
            if (!src.ExpectTokenString("}") 
              return false;
            }
            return true;
          }
          else if (token == "portals" 
            int numPortals, i;
            aasPortal_t portal;
            numPortals = src.ParseInt;
            portals.Resize(numPortals);
            if (!src.ExpectTokenString("{") 
              return false;
            }
            for (i = 0; i < numPortals; i++ 
              src.ParseInt;
              src.ExpectTokenString("(");
              portal.areaNum = src.ParseInt;
              portal.clusters[0] = src.ParseInt;
              portal.clusters[1] = src.ParseInt;
              portal.clusterAreaNum[0] = src.ParseInt;
              portal.clusterAreaNum[1] = src.ParseInt;
              src.ExpectTokenString(")");
              portals.Append(portal);
            }
            if (!src.ExpectTokenString("}") 
              return false;
            }
            return true;
          }
          else if (token == "clusters" 
            int numClusters, i;
            aasCluster_t cluster;

            numClusters = src.ParseInt;
            clusters.Resize(numClusters);
            if (!src.ExpectTokenString("{") 
              return false;
            }
            for (i = 0; i < numClusters; i++ 
              src.ParseInt;
              src.ExpectTokenString("(");
              cluster.numAreas = src.ParseInt;
              cluster.numReachableAreas = src.ParseInt;
              cluster.firstPortal = src.ParseInt;
              cluster.numPortals = src.ParseInt;
              src.ExpectTokenString(")");
              clusters.Append(cluster);
            }
            if (!src.ExpectTokenString("}") 
              return false;
            }
            return true;
          }
          else {
            raise Currupt;
          }
        }
        FinishAreas;
        depth = MaxTreeDepth;
        if (depth > MAX_AAS_TREE_DEPTH 
          src.Error("idAASFileLocal::Load: tree depth = %d", depth);
        }
        common->UpdateLevelLoadPacifier;
        common->Printf("done.\n");
        return true;
      end To_Internal;
    function To_Internal return Record_Model is
      idList<idVec2> texCoords;
      idList<int> firstWeightForVertex;
      idList<int> numWeightsForVertex;
      idList<int> tris;
      int numWeights = 0;
      int maxweight = 0;
      static int maxWeightsPerVert;
      static float maxResidualWeight;
      const int MAX_VERTEX_WEIGHTS = 4;
      idList< bool > jointIsUsed;
      jointIsUsed.SetNum(numJoints); := (others => False);
      numMeshJoints = 0;
      maxJointVertDist = 0.0f;
      idVec4 * scaledWeights = (idVec4 *) Mem_Alloc16(numWeights * sizeof(scaledWeights[0]), TAG_MD5_WEIGHT);
      int * weightIndex = (int *) Mem_Alloc16(numWeights * 2 * sizeof(weightIndex[0]), TAG_MD5_INDEX);
      memset(weightIndex, 0, numWeights * 2 * sizeof(weightIndex[0]));
      begin
        Data.Assert_Next("Version"); Animation.Version := Integer_4_Positive'wide_value(Data.Next);
        Data.Assert_Next("commandline"); Animation.Command_Line := Data.Next;
        Data.Assert_Next("numJoints"); Animation.Command_Line := Data.Next;
        Data.Assert_Next("numMeshes"); Animation.Command_Line := Data.Next; Natural
        Data.Assert_Next("joints", "{");
        idJointMat *poseMat = ( idJointMat * )_alloca16( joints.Num() * sizeof( poseMat[0] ) );
        for( int i = 0; i < joints.Num(); i++ ) {
          idMD5Joint * joint = &joints[i];
          idJointQuat * pose = &defaultPose[i];
          ParseJoint( parser, joint, pose );
          -- parse name
          idToken token;
          parser.ReadToken( &token );
          joint->name = token;
          -- parse parent
          int num = parser.ParseInt();
          if ( num < 0 ) {
            joint->parent = NULL;
          } else {
            if ( num >= joints.Num() - 1 ) {
              parser.Error( "Invalid parent for joint '%s'", joint->name.c_str() );
            }
            joint->parent = &joints[ num ];
          }
          -- parse default pose
          parser.Parse1DMatrix( 3, defaultPose->t.ToFloatPtr() );
          parser.Parse1DMatrix( 3, defaultPose->q.ToFloatPtr() );
          defaultPose->q.w = defaultPose->q.CalcW();
          poseMat[ i ].SetRotation( pose->q.ToMat3() );
          poseMat[ i ].SetTranslation( pose->t );
          if ( joint->parent ) {
            parentNum = joint->parent - joints.Ptr();
            pose->q = ( poseMat[ i ].ToMat3() * poseMat[ parentNum ].ToMat3().Transpose() ).ToQuat();
            pose->t = ( poseMat[ i ].ToVec3() - poseMat[ parentNum ].ToVec3() ) * poseMat[ parentNum ].ToMat3().Transpose();
          }
        }
        parser.ExpectTokenString( "}" );
        -------------------------------------------
        -- create the inverse of the base pose joints to support tech6 style deformation
        -- of base pose vertexes, normals, and tangents.
        --
        -- vertex * joints * inverseJoints == vertex when joints is the base pose
        -- When the joints are in another pose, it gives the animated vertex position
        -------------------------------------------
        invertedDefaultPose.SetNum( SIMD_ROUND_JOINTS( joints.Num() ) );
        for ( int i = 0; i < joints.Num(); i++ ) {
          invertedDefaultPose[i] = poseMat[i];
          invertedDefaultPose[i].Invert();
        }
        SIMD_INIT_LAST_JOINT( invertedDefaultPose.Ptr(), joints.Num() );
        for ( int i = 0; i < meshes.Num(); i++ ) {
          parser.ExpectTokenString( "mesh" );
          meshes[i].ParseMesh( parser, defaultPose.Num(), poseMat );
          Data.Assert_Next("{", "Name"); Model.Name := To_String_2_Unbounded(Data.Next);
          Data.Assert_Next("shader"     ; Model.Shader := To_String_2_Unbounded(Data.Next);
          Data.Assert_Next("numverts"   ; Animation.Verticies.Set_Size(Integer_4_Natural'wide_value(Data.Next));
          for Texture_Coordinate of Texture_Coordinates loop
            Data.Assert_Next("vert"; Data.Next;
            parser.Parse1DMatrix(2, texCoords[ i ].ToFloatPtr);
            First_Weight_For_Vertex := Integer_4_Natural'wide_value(Data.Next);
            Number_Of_Weights_For_Vertex := Integer_4_Positive'wide_value(Data.Next);
            Number_of_Weights := Number_of_Weights + Number_Of_Weights_For_Vertex;
            if Number_Of_Weights_For_Vertex + Number_Of_Weights_For_Vertex > Maximum_Weight then
              Maximum_Weight := Number_Of_Weights_For_Vertex + Number_Of_Weights_For_Vertex;
            end if;
          end loop;
          Data.Assert_Next("numtris"; Number_Of_Triangles := Integer_4_Natural'wide_value(Data.Next);
          tris.SetNum(count * 3);
          for Triangle of Trinagles loop
            Data.Assert_Next("tri"; Data.Next;
            tris[ i * 3 + 0 ] = parser.ParseInt;
            tris[ i * 3 + 1 ] = parser.ParseInt;
            tris[ i * 3 + 2 ] = parser.ParseInt;
          end loop;
          for Weight of Temp_Weights loop
            Data.Assert_Next("weight"; Data.Next;
            tempWeights[ i ].joint      = Integer_4_Natural'wide_value(Data.Next);
            tempWeights[ i ].jointWeight  = parser.ParseFloat;
            parser.Parse1DMatrix(3, tempWeights[ i ].offset.ToFloatPtr);
            if tempWeights[ i ].joint >= Number_Of_Joints;
          end loop;
            declare
            begin
              Total := 0;
              for I in 1..Number_Of_Weights_For_Verticies loop
                Current := firstWeightForVertex[i];
                for J in 1..Number_Of_Weights_For_Verticies(I) loop
                  scaledWeights[Total].ToVec3 = tempWeights[Current].offset * tempWeights[Current].jointWeight;
                  scaledWeights[Total].w = tempWeights[Current].jointWeight;
                  weightIndex[Total * 2 + 0] = tempWeights[Current].joint * sizeof(idJointMat);
                end loop;
                weightIndex[count * 2 - 1] = 1;
              end loop;
              Data.Assert_Next("}";
              for I in 1..numWeights loop
                c_numWeightJoints := c_numWeightJoints + weightIndex[i*2+1];
              end loop;
              idDrawVert * basePose = (idDrawVert *)Mem_ClearedAlloc(texCoords.Num * sizeof(*basePose), TAG_MD5_BASE);
              J := 0;
              for I in 1..TexCoors.Num loop
                idVec3 v = (*(idJointMat *) ((byte *)joints + weightIndex[j*2+0])) * scaledWeights[j];
                while weightIndex[j*2+1] = 0 loop
                  j++;
                  v += (*(idJointMat *) ((byte *)joints + weightIndex[j*2+0])) * scaledWeights[j];
                end loop;
                j++;
                basePose[i].Clear;
                basePose[i].xyz = v;
                basePose[i].SetTexCoord(texCoords[i]);
              end loop;
              numMeshJoints = 0;
              maxJointVertDist = 0.0f;
              -----------------------------------------------------------------------------
              -- New-style setup for fixed four weights and normal / tangent deformation --
              -- Several important models have >25% residual weight in joints after the  --
              -- first four, which is worrisome for using a fixed four joint deformation --
              -----------------------------------------------------------------------------
            end;
            for (int i = 0; i < texCoords.Num; i++ 
              idDrawVert & dv = basePose[i];
            -- some models do have >4 joint weights, so it is necessary to sort and renormalize
            -- sort the weights and take the four largest
            int weights[256];
            const int numWeights = numWeightsForVertex[ i ];
            for (int j = 0; j < numWeights; j++ 
              weights[j] = firstWeightForVertex[i] + j;
            }
            -- bubble sort
            for (int j = 0; j < numWeights; j++ 
              for (int k = 0; k < numWeights - 1 - j; k++ 
                if (tempWeights[weights[k]].jointWeight < tempWeights[weights[k+1]].jointWeight 
                  SwapValues(weights[k], weights[k+1]);
                }
              }
            }
            if (numWeights > maxWeightsPerVert 
              maxWeightsPerVert = numWeights;
            }
            const int usedWeights = Min(MAX_VERTEX_WEIGHTS, numWeights);
            float totalWeight = 0;
            for (int j = 0; j < numWeights; j++ 
              totalWeight += tempWeights[weights[j]].jointWeight;
            }
            assert(totalWeight > 0.999f && totalWeight < 1.001f);
            float usedWeight = 0;
            for (int j = 0; j < usedWeights; j++ 
              usedWeight += tempWeights[weights[j]].jointWeight;
            }
            const float residualWeight = totalWeight - usedWeight;
            if (residualWeight > maxResidualWeight 
              maxResidualWeight = residualWeight;
            }
            byte finalWeights[MAX_VERTEX_WEIGHTS] = { 0 };
            byte finalJointIndecies[MAX_VERTEX_WEIGHTS] = { 0 };
            for (int j = 0; j < usedWeights; j++ 
              const vertexWeight_t & weight = tempWeights[weights[j]];
              const int jointIndex = weight.joint;
              const float fw = weight.jointWeight;
              assert(fw >= 0.0f && fw <= 1.0f);
              const float normalizedWeight = fw / usedWeight;
              finalWeights[j] = idMath::Ftob(normalizedWeight * 255.0f);
              finalJointIndecies[j] = jointIndex;
            }
            -- Sort the weights and indices for hardware skinning
            for (int k = 0; k < 3; ++k 
              for (int l = k + 1; l < 4; ++l 
                if (finalWeights[l] > finalWeights[k] 
                  SwapValues(finalWeights[k], finalWeights[l]);
                  SwapValues(finalJointIndecies[k], finalJointIndecies[l]);
                }
              }
            }

            -- Give any left over to the biggest weight
            finalWeights[0] += Max(255 - finalWeights[0] - finalWeights[1] - finalWeights[2] - finalWeights[3], 0);

            dv.color[0] = finalJointIndecies[0];
            dv.color[1] = finalJointIndecies[1];
            dv.color[2] = finalJointIndecies[2];
            dv.color[3] = finalJointIndecies[3];
            dv.color2[0] = finalWeights[0];
            dv.color2[1] = finalWeights[1];
            dv.color2[2] = finalWeights[2];
            dv.color2[3] = finalWeights[3];
            for (int j = usedWeights; j < 4; j++ 
              assert(dv.color2[j] == 0);
            }
            for (int j = 0; j < usedWeights; j++ 
              if (!jointIsUsed[finalJointIndecies[j]] 
                jointIsUsed[finalJointIndecies[j]] = true;
                numMeshJoints++;
              }
              const idJointMat & joint = joints[finalJointIndecies[j]];
              float dist = (dv.xyz - joint.GetTranslation).Length;
              if (dist > maxJointVertDist 
                maxJointVertDist = dist;
              }
            }
          }
          meshJoints = (byte *) Mem_Alloc(numMeshJoints * sizeof(meshJoints[0]), TAG_MODEL);
          numMeshJoints = 0;
          for (int i = 0; i < numJoints; i++ 
            if (jointIsUsed[i] 
              meshJoints[numMeshJoints++] = i;
            }
          }
          Mem_Free(basePose);
        end loop;
        -- calculate the bounds of the model
        bounds.Clear();
        for ( int i = 0; i < meshes.Num(); i++ ) {
          idBounds meshBounds;
          meshes[i].CalculateBounds( poseMat, meshBounds );
          bounds.AddBounds( meshBounds );
        }
        return Model;
      end To_Internal;
    function To_Internal return Record_Animation is
      begin
        idStr generatedFileName = "generated/anim/";
        generatedFileName.AppendPath(filename);
        generatedFileName.SetFileExtension(".MD5anim"); -- Get the timestamp on the original file, if it's newer than what is stored in binary model, regenerate it
        ID_TIME_T sourceTimeStamp = fileSystem->GetTimestamp(filename);
        idFileLocal file(fileSystem->OpenFileReadMemory(generatedFileName));
        if binaryLoadAnim.GetBoo and LoadBinary(file, sourceTimeStamp) then
          name = filename;
          if (cvarSystem->GetCVarBool("fs_buildresources") fileSystem->AddAnimPreload(name); -- for resource gathering write this anim to the preload file for this map
          return true;
        end if;
        if (!parser.LoadFile(filename) return false; }
        name = filename;
        Data.Assert_Next("Version"); Animation.Version := Integer_4_Positive'wide_value(Data.Next);
        Data.Assert_Next("commandline"); Animation.Command_Line := Data.Next;
        Data.Assert_Next("numFrames"); Number_Of_Frames Integer_4_Positive'wide_value(Data.Next));
        Data.Assert_Next("numJoints"); Animation.Joints.Set_Size(Integer_4_Positive'wide_value(Data.Next));
        Data.Assert_Next("frameRate"); Animation.Frame_Rate := Integer_4_Natural'wide_value(Data.Next);
        Data.Assert_Next("numAnimatedComponents"); Animation.Number_Of_Components := Integer_4_Natural'wide_value(Data.Next);
        if Animation.Number_Of_Components > Animation.Joints.Get_Size * 6 then raise Currupt; end if;
        Data.Assert_Next("heirarchy", "{");
          declare
          Animation : Record_Animation(Number_Of_Frames, Number_Of_Joints) := (others => <>);
          begin
            for State of Animation.States loop
              State.jointInfo[ i ].nameIndex = animationLib.JointIndex(token);
              jointInfo[ i ].parentNum = parser.ParseInt;
              if (jointInfo[ i ].parentNum >= i 
              if ((i != 0) && (jointInfo[ i ].parentNum < 0) 
              -- parse anim bits
              jointInfo[ i ].animBits = parser.ParseInt;
              if (jointInfo[ i ].animBits & ~63 
              -- parse first component
              jointInfo[ i ].firstComponent = parser.ParseInt;
              if ((numAnimatedComponents > 0) && ((jointInfo[ i ].firstComponent < 0) || (jointInfo[ i ].firstComponent >= numAnimatedComponents)) 
            end loop;
            Data.Assert_Next("}", "bounds", "{");
            for State of Animation.States loop
              parser.Parse1DMatrix(3, bounds[ i ][ 0 ].ToFloatPtr);
              parser.Parse1DMatrix(3, bounds[ i ][ 1 ].ToFloatPtr);
            end loop;
            Data.Assert_Next("}", "baseframe", "{");
            for State of Animation.States loop
              idCQuat q;
              parser.Parse1DMatrix(3, baseFrame[ i ].t.ToFloatPtr);
              parser.Parse1DMatrix(3, q.ToFloatPtr);--baseFrame[ i ].q.ToFloatPtr);
              baseFrame[ i ].q = q.ToQuat;--.w = baseFrame[ i ].q.CalcW;
              baseFrame[ i ].w = 0.0f;
            end loop;
            Data.Assert_Next("}");
            for State of Animation.States loop
              Data.Assert_Next("frame");
              if Animation.States.Get_Size /= Integer_4_Positive'wide_value(Data.Next) then raise Currupt; end if;
              Data.Assert_Next("{");
              for I in 1..Animation.Number_Of_Components loop State.Components.Add(parser.ParseFloat); end loop;
            end loop;
            if Animation.Components.Element(1).Has_X then
              for I in 1..Number_Of_Components loop componentPtr[ numAnimatedComponents * i ] -= baseFrame[ 0 ].t.x; end loop;
              Total_Delta.x = componentPtr[ numAnimatedComponents * (numFrames - 1) ];
            else
              totaldelta.x = 0.0f;
            end if;
            baseFrame[ 0 ].t.Zero;
            -- we don't count last frame because it would cause a 1 frame pause at the end
            animLength = ((numFrames - 1) * 1000 + frameRate - 1) / frameRate;
            return Animation;
          end;
      end To_Internal;
    function To_Internal return Record_Camera is
      begin
        Data.Assert_Next("Version");
        if Integer_4_Positive'wide_value(Data.Next) not in MINIMUM_VERSION..MAXIMUM_VERSION then raise Unsupported_Feature; end if;
        Data.Assert_Next("commandline"); Data.Skip; 
        Data.Assert_Next("numFrames");   Number_Of_Frames := Integer_4_Natural'wide_value(Data.Next));
        Data.Assert_Next("frameRate");   Frame_Rate       := Integer_4_Natural'wide_value(Data.Next);
        Data.Assert_Next("numCuts");     Number_Of_Cuts   := Integer_4_Positive'wide_value(Data.Next);
        if Camera.Number_Of_Cuts > Camera.Number_Of_Frames;
          declare
          Camera : Record_Camera(Number_Of_Frames, Number_Of_Cuts) := (others => <>);
          begin
            Data.Assert_Next("cuts", "{");
            for Cut of Camera.Cuts loop
              Cut := Integer_4_Positive'wide_value(Data.Next);
              if Cut >= Camera.Frames'size;
            end loop;
            Data.Assert_Next("}", "camera", "{");
            for Frame of Camera.Frames loop
              Frames :=
                Integer_4_Positive'wide_value(Data.Next)
                Parse1DMatrix(3, camera[ i ].t.ToFloatPtr 
                parser.Parse1DMatrix(3, camera[ i ].q.ToFloatPtr 
                camera[ i ].fov = parser.ParseFloat;));
            end loop;
            Data.Assert_Next("}");
            Data.Assert_End;
            return Camera;
          end;
      end To_Internal;
  end Id_Software;
