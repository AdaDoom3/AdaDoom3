
--                                                                                                                                      --
--                                                         N E O  E N G I N E                                                           --
--                                                                                                                                      --
--                                                 Copyright (C) 2016 Justin Squirek                                                    --
--                                                                                                                                      --
-- Neo is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the --
-- Free Software Foundation, either version 3 of the License, or (at your option) any later version.                                    --
--                                                                                                                                      --
-- Neo is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of                --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.                            --
--                                                                                                                                      --
-- You should have received a copy of the GNU General Public License along with Neo. If not, see gnu.org/licenses                       --
--                                                                                                                                      --

-- Shadow stencil shading with prenumbra: https://web.archive.org/web/20160417154820/http://www.terathon.com/gdc05_lengyel.pdf
separate (Neo.Engine.Renderer) procedure Frontend is

  -- Amount of idle frontend light job tasks that are allowed to sit idle until tasks start being destroyed
  MAX_IDLE_LIGHT_JOBS : constant Positive := 10;
  FOG_SCALE           : constant Float    := 0.001;

  -- ???
  Area : ;
  BSP  : ;
  Pos  : ;

  -----------------------
  -- Light check tasks --
  -----------------------
  --
  --
  --

  procedure Run_Light_Check is
    begin
      loop
        View.View_Entities.Append (Render_Lights.Shadow_Only_View_Entities);

        -- Evaluate light shader registers

        -- Additve lights with no stage in the light shader is positive, remove the light
        if Light.Kind := Fog_Light or Light.Kind := Blend_Light then
          Outter: for Stage of Light.Stages loop
            if Light.Stage.Registers.Condition_Register then

              -- Snap tiny values to zero
              for Register of Light.Stage.Registers loop
                if Register < 0.001 then Register := 0.0; end if;
              end loop;

              -- We found a light that add something
              for Register of Light.Stage.Registers loop
                exit Outter when Register > 0.0;
              end loop;              
            end if;
            if Stage := Light.Stages.Last then 

-- We didn't find any addition to the scene
goto Skip_Light;
          end if;
        end loop Outter;
      end if;

      -- Copy data to backend

      -- Create a "fog plane" to represent light far clip plane

      -- Calculate the matrix that projects the zero-to-one cube to exactly cover the light frustum in clip space

      -- Calculate the project bounds, either not clipped at all, near clipped, or fully clipped
        
      -- Build the scissor

      -- Create interactions with all entities the light may touch and add entities that may cast shadows
      for Reference of Light.References loop

        -- Some lights have their center of projection outside of the world so ignore areas not connected to the light center
        if not Light.World.Areas_Are_Connected (Light.Area, Reference.Area, PS_BLOCK_VIEW) then

          -- Check the models in this area
          for Model of Reference.Entities loop
            if not (vLight.entityInteractionState[ edef.index ] /= viewLight_t::INTERACTION_UNCHECKED or else
                        
              -- A large fraction of static entity / light pairs will still have no interactions even though they are both present in the same area(s)
              eModel /= NULL && !eModel.IsDynamicModel && inter = INTERACTION_EMPTY or else
                        
                -- We don't want the lights on weapons to illuminate anything else. There are two assumptions here - that
                -- allowLightInViewID is only used for weapon lights, and that all weapons will have weaponDepthHack. A more general
                -- solution would be to have an allowLightOnEntityID field.
                light.parms.allowLightInViewID && light.parms.pointLight && !eParms.weaponDepthHack or else

                --
                (eParms.noShadow or ( eModel && !eModel.ModelHasShadowCastingSurfaces ) ) && !edef.IsDirectlyVisible or else
                eModel && !eModel.ModelHasInteractingSurfaces && !eModel.ModelHasShadowCastingSurfaces or else
                inter = NULL and (eParms.noDynamicInteractions or R_CullModelBoundsToLight( light, edef.localReferenceBounds, edef.modelRenderMatrix )) or else 
                edef.IsDirectlyVisible 
                !lightCastsShadows 
            then

            end if;
          end loop;
        end if;
      end loop;
    end;

  -------------------
  -- Flood_Portals --
  -------------------
  --
  -- Recursivly find lights and entities by flowing from an origin through visible portals that the view origin can see into
  --

  procedure Flood_Portals (Origin : Vector_3D; Planes : in out Vector_Plane_4D.Unsafe.Vector) is
    portalStack_t ps;
    ps.next := NULL;
    ps.p := NULL;
    idScreenRect r;
    idVec3 v;
    idVec3 ndc;
    begin
      Portals.Planes := Planes;
      Portals.Rectangle := View.Scissor;

      -- If we are outside the world, mark everything
      if View.Area < 0 then
        for I in 1..Length (Planes) loop
          Area_Screen_Rect (I) := View.Scissor;
          Portals.Add_Area_To_View (Portal)
        end loop;
      else
        portalArea_t * area := &portalAreas[ areaNum ];

        -- Cull models and lights to the current collection of planes
        AddAreaToView ( areaNum, ps);
        Area_Screen_Rect (Area) := (if Is_Empty (Area_Screen_Rect (Area)) then Portals.Rect;
                                    else Area_Screen_Rect (Area) := Join (Portals.Rect, Area_Screen_Rect (Area)));

        -- Go through all the portals
        for Portal of Portals loop
          
          -- Check the portal isn't a closed door and that it is facing the view
          Distance := Portal.Plane.Distance (Origin);
          if not Portal.Double_Portal.Is_View_Block and Distance < -0.1 then

            -- If we are very close to the portal surface, avoid clipping that may cause areas to vanish
            if Distance < 1.0 then
              New_Portals := Portals;
              New_Portals.Append (Portal);
              Flood_View_Through_Portals (Origin, New_Portals);
            else

              -- Clip the portal winding to all of the planes
              for Plane of Portal.Planes loop exit when not Is_Clip_In_Place (Portal.Winding, Plane); end loop;

              -- Continue only if the portal is visible and not fogged out
              if Portal.Winding.Points.Length /= 0 and Is_Fogged (Portal) then

                -- Go through the portal
                New_Portals.Append (Portal);

                -- Scissor things outside the screen pixel bounding box
                New_Portals.Rect := Screen_Rect_From_Winding (Portal.Winding, Identity_Space);
                View_Width  := View.Port.X2 - View.Port.X1;
                View_Height := View.Port.Y2 - View.Port.Y1;
                for Point of Portal.Winding.Points loop
                  R_LocalPointToGlobal (space.modelMatrix, (*w)[i].ToVec3(), v);
                  R_GlobalToNormalizedDeviceCoordinates (v, ndc);
                  Screen_Rect.Points.Append (X => (ndc[0] * 0.5 + 0.5) * viewWidth,
                                             Y => (ndc[1] * 0.5 + 0.5) * viewHeight);
                end loop;
                r.Expand;

                -- Slop might have spread it a pixel outside, so trim it back
                New_Portals.Rect.Intersect (Portals.Rect);

                -- Generate a set of clipping planes to restrict the view visibility beyond the scissor
                for Point of Winding.Points loop
                  newStack.portalPlanes[newStack.numPortalPlanes].Normal().Cross( origin - w[j].ToVec3(), origin - w[i].ToVec3());

                  -- Skip degenerates
                  if newStack.portalPlanes[newStack.numPortalPlanes].Normalize() >= 0.01f then
                    newStack.portalPlanes[newStack.numPortalPlanes].FitThroughPoint( origin);
                  end if;
                end loop;
              end if;
            end if;
          end if;

          -- 
          newStack.portalPlanes[newStack.numPortalPlanes] := p.plane;
          Flood_View_Through_Portals (origin, Portal.Into_Area, New_Portals);
        end loop;
      end if;
    end;

  ----------
  -- Main --
  ----------
  --
  --
  --

  begin
    for View of Views loop

      -- Setup the view matrix
      Viewer := (Axis.XX, Axis.YX, Axis.ZX, -Origin.X * Axis.XX - Origin.Y * YX - Origin.Z * Axis.ZX,
                 Axis.XY, Axis.YY, Axis.ZY, -Origin.X * Axis.XY - Origin.Y * YY - Origin.Z * Axis.ZY,
                 Axis.XZ, Axis.YZ, Axis.ZZ, -Origin.X * Axis.XZ - Origin.Y * YZ - Origin.Z * Axis.ZZ);

--         -- Setup the projection matrix (use the ol' "infinite far z trick")
--         Jitter_Values := (if Jitter.Get then (Random_Float, Random_Float) else  (others => <>));
--         R_SetupProjectionMatrix

      -- Setup render matricies for faster culliong
      View.Render_Projection := Transpose (View.Projection);
      View.World_Space.MVP := View.Render_Projection * Transpose (View.World_Space.Model_View);
      View.Frustum := ((4)(3) => Z_Near.Get, others => -Get_Frustum_Planes (View.World_Space.MVP));

      -- Walk the BSP tree to find the area where the view origin is
      BSP := View.Level.Partitions.Get;
      Pos := BSP.Root;
      loop
        Node := Get (Pos);
        case Node.Kind is
          when Area_Partition   => Area := Node.Area_Id; exit;
          when Opaque_Partition => raise In_Solid;
          when Normal_Partition => Pos := (if View.Origin * Normal (Node.Plane) + Node.Plane.W > 0 then First (Pos) else Last (Pos));
        end case;
      end loop;

      -- Flood through portals
      Flood_Portals (Origin, Area, View.Planes);

      -- Reset the amount of light jobs if changed (this will need tuning)
      if    Length (View.Lights) < Length (Light_Jobs)                       then Light_Jobs.Append (new Vector_Lights.Unsafe.Vector (Length (View.Lights) - Length (Light_Jobs)));
      elsif Length (View.Lights) + MAX_IDLE_LIGHT_JOBS > Length (Light_Jobs) then Light_Jobs.Remove (1..Length (View.Lights) + MAX_IDLE_LIGHT_JOBS - Length (Light_Jobs)); end if;

      -- Kick-off light projection and searching for lit models then wait for them to finish
      for I in 1..Length (View.Lights) loop
        Light_Jobs.Element (I).Do_Pass (View.Lights.Element (I));
      end loop;
      for Light_Job of Light_Jobs loop
        Light_Job.Confirm_Completion;
      end loop;

--         -- Determine all possible connected areas for light-behind-door culling
--         if Area := OUTSIDE_AREA then View.Connected_Areas := (others => True)
--         else View.Level.Portals.Iterate_Subtree (Flood_Portals); end if;

      -- R_SortDrawSurfs            -- A simple C qsort call. C++ sort would have been faster thanks to inlining.       
      -- R_GenerateSubViews
      -- R_AddDrawViewCmd 
    end loop;
  end;
