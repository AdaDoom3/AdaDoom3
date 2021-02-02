
--                                                                                                                               --
--                                                      N E O  E N G I N E                                                       --
--                                                                                                                               --
--                                               Copyright (C) 2020 Justin Squirek                                               --
--                                                                                                                               --
-- Neo is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published --
-- by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.                      --
--                                                                                                                               --
-- Neo is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of         --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.                     --
--                                                                                                                               --
-- You should have received a copy of the GNU General Public License along with Neo. If not, see gnu.org/licenses                --
--                                                                                                                               --

package body Neo.World.Geometry is

  ---------------
  -- Constants --
  ---------------

  A : constant := 0.5; -- 1 / 2
  B : constant := 0.866025; -- Root (3) / 2

  CYLINDER_MESH_VERTICIES : constant := (1.0,  0.0, 1.0,  1.0,  0.0, 1.0,  1.0,  0.0, 0.0,  1.0,  0.0, 0.0,
                                           B,    A, 1.0,    B,    A, 1.0,    B,    A, 0.0,    B,    A, 0.0,
                                           A,    B, 1.0,    A,    B, 1.0,    A,    B, 0.0,    A,    B, 0.0,
                                         0.0,  1.0, 1.0,  0.0,  1.0, 1.0,  0.0,  1.0, 0.0,  0.0,  1.0, 0.0,
                                          -A,    B, 1.0,   -A,    B, 1.0,   -A,    B, 0.0,   -A,    B, 0.0,
                                          -B,    A, 1.0,   -B,    A, 1.0,   -B,    A, 0.0,   -B,    A, 0.0,
                                        -1.0,  0.0, 1.0, -1.0,  0.0, 1.0, -1.0,  0.0, 0.0, -1.0,  0.0, 0.0,
                                          -B,   -A, 1.0,   -B,   -A, 1.0,   -B,   -A, 0.0,   -B,   -A, 0.0,
                                          -A,   -B, 1.0,   -A,   -B, 1.0,   -A,   -B, 0.0,   -A,   -B, 0.0,
                                         0.0, -1.0, 1.0,  0.0, -1.0, 1.0,  0.0, -1.0, 0.0,  0.0, -1.0, 0.0,
                                           A,   -B, 1.0,    A,   -B, 1.0,    A,   -B, 0.0,    A,   -B, 0.0,
                                           B,   -A, 1.0,    B,   -A, 1.0,    B,   -A, 0.0,    B,   -A, 0.0,
                                         1.0,  0.0, 1.0,  1.0,  0.0, 1.0,  1.0,  0.0, 0.0,  1.0,  0.0, 0.0);

  CYLINDER_TOP_CAP_MESH_VERTICIES : constant := (B,   -A, 1.0, 0.0, 1.0, 1.0,
                                               0.0,  1.0, 1.0, 0.0, 1.0, 1.0,
                                                 A,   -B, 1.0, 0.0, 1.0, 1.0,
                                                 A,   -B, 1.0, 0.0, 1.0, 1.0,
                                               0.0,  1.0, 1.0, 0.0, 1.0, 1.0,
                                               0.0, -1.0, 1.0, 0.0, 1.0, 1.0,
                                               0.0, -1.0, 1.0, 0.0, 1.0, 1.0,
                                               0.0,  1.0, 1.0, 0.0, 1.0, 1.0,
                                                -A,   -B, 1.0, 0.0, 1.0, 1.0,
                                                -A,   -B, 1.0, 0.0, 1.0, 1.0,
                                               0.0,  1.0, 1.0, 0.0, 1.0, 1.0,
                                                -B,   -A, 1.0, 0.0, 1.0, 1.0,
                                                -B,   -A, 1.0, 0.0, 1.0, 1.0,
                                               0.0,  1.0, 1.0, 0.0, 1.0, 1.0,
                                              -1.0,  0.0, 1.0, 0.0, 1.0, 1.0,
                                              -1.0,  0.0, 1.0, 0.0, 1.0, 1.0,
                                               0.0,  1.0, 1.0, 0.0, 1.0, 1.0,
                                                -B,    A, 1.0, 0.0, 1.0, 1.0,
                                                -B,    A, 1.0, 0.0, 1.0, 1.0,
                                               0.0,  1.0, 1.0, 0.0, 1.0, 1.0,
                                                -A,    B, 1.0, 0.0, 1.0, 1.0,
                                                -A,    B, 1.0, 0.0, 1.0, 1.0,
                                               0.0,  1.0, 1.0, 0.0, 1.0, 1.0,
                                               0.0,  1.0, 1.0, 0.0, 1.0, 1.0,
                                               0.0,  1.0, 1.0, 0.0, 1.0, 1.0,
                                               0.0,  1.0, 1.0, 0.0, 1.0, 1.0,
                                                 A,    B, 1.0, 0.0, 1.0, 1.0,
                                                 A,    B, 1.0, 0.0, 1.0, 1.0,
                                               0.0,  1.0, 1.0, 0.0, 1.0, 1.0,
                                                 B,    A, 1.0, 0.0, 1.0, 1.0,
                                                 B,    A, 1.0, 0.0, 1.0, 1.0,
                                               0.0,  1.0, 1.0, 0.0, 1.0, 1.0,
                                               1.0,  0.0, 1.0, 0.0, 1.0, 1.0,
                                               1.0,  0.0, 1.0, 0.0, 1.0, 1.0,
                                               0.0,  1.0, 1.0, 0.0, 1.0, 1.0,
                                                 B,   -A, 1.0, 0.0, 1.0, 1.0);

  CYLINDER_BOTTOM_CAP_MESH_VERTICIES : constant := (1.0,  0.0, 0.0, 0.0, -1.0, 0.0,
                                                    0.0,  0.0, 0.0, 0.0, -1.0, 0.0,
                                                      B,    A, 0.0, 0.0, -1.0, 0.0,
                                                      B,    A, 0.0, 0.0, -1.0, 0.0,
                                                    0.0,  0.0, 0.0, 0.0, -1.0, 0.0,
                                                      A,    B, 0.0, 0.0, -1.0, 0.0,
                                                      A,    B, 0.0, 0.0, -1.0, 0.0,
                                                    0.0,  0.0, 0.0, 0.0, -1.0, 0.0,
                                                    0.0,  1.0, 0.0, 0.0, -1.0, 0.0,
                                                    0.0,  1.0, 0.0, 0.0, -1.0, 0.0,
                                                    0.0,  0.0, 0.0, 0.0, -1.0, 0.0,
                                                     -A,    B, 0.0, 0.0, -1.0, 0.0,
                                                     -A,    B, 0.0, 0.0, -1.0, 0.0,
                                                    0.0,  0.0, 0.0, 0.0, -1.0, 0.0,
                                                     -B,    A, 0.0, 0.0, -1.0, 0.0,
                                                     -B,    A, 0.0, 0.0, -1.0, 0.0,
                                                    0.0,  0.0, 0.0, 0.0, -1.0, 0.0,
                                                   -1.0,  0.0, 0.0, 0.0, -1.0, 0.0,
                                                   -1.0,  0.0, 0.0, 0.0, -1.0, 0.0,
                                                    0.0,  0.0, 0.0, 0.0, -1.0, 0.0,
                                                     -B,   -A, 0.0, 0.0, -1.0, 0.0,
                                                     -B,   -A, 0.0, 0.0, -1.0, 0.0,
                                                    0.0,  0.0, 0.0, 0.0, -1.0, 0.0,
                                                     -A,   -B, 0.0, 0.0, -1.0, 0.0,
                                                     -A,   -B, 0.0, 0.0, -1.0, 0.0,
                                                    0.0,  0.0, 0.0, 0.0, -1.0, 0.0,
                                                    0.0, -1.0, 0.0, 0.0, -1.0, 0.0,
                                                    0.0, -1.0, 0.0, 0.0, -1.0, 0.0,
                                                    0.0,  0.0, 0.0, 0.0, -1.0, 0.0,
                                                      A,   -B, 0.0, 0.0, -1.0, 0.0,
                                                      A,   -B, 0.0, 0.0, -1.0, 0.0,
                                                    0.0,  0.0, 0.0, 0.0, -1.0, 0.0,
                                                      B,   -A, 0.0, 0.0, -1.0, 0.0,
                                                      B,   -A, 0.0, 0.0, -1.0, 0.0,
                                                    0.0,  0.0, 0.0, 0.0, -1.0, 0.0,
                                                    1.0,  0.0, 0.0, 0.0, -1.0, 0.0);

  ---------------
  -- Make_Line --
  ---------------

  procedure Make_Line (P0, P1, Color : Point_3D; Result : Ptr_Mesh_State) is
    begin
      Result.Verticies := ;
	glColor4f(color.x, color.y, color.z, 1.0f);
	Point av3LineEndpoints[] = {p0, p1};
	glEnableClientState(GL_VERTEX_ARRAY);

	glVertexPointer(3, GL_FLOAT, sizeof(Point), &av3LineEndpoints[0].x);
	glDrawArrays(GL_LINES, 0, 2);
	glDisableClientState(GL_VERTEX_ARRAY);
	glColor4f(1.0f, 1.0f, 1.0f, 1.0f);
    end;
  procedure Make_Circle (udword nb_segments, const Matrix4x4& matrix, const Point& color, float radius, bool semi_circle=false) is
    begin

        float step = TWOPI / float(nb_segments);
	udword segs = nb_segments;
	if(semi_circle)
		segs /= 2;

	Point* tmp = (Point*)StackAlloc(sizeof(Point)*segs*2);
	for(udword i=0;i<segs;i++)
		udword j=i+1;
		if(j==nb_segments)
			j=0;

		const float angle0 = float(i) * step;
		const float angle1 = float(j) * step;

		const Point p0 = Point(radius * sinf(angle0), radius * cosf(angle0), 0.0f) * matrix;
		const Point p1 = Point(radius * sinf(angle1), radius * cosf(angle1), 0.0f) * matrix;

//		DrawLine(p0, p1, color);
		tmp[i*2+0] = p0;
		tmp[i*2+1] = p1;
	}
	glColor4f(color.x, color.y, color.z, 1.0f);
	glEnableClientState(GL_VERTEX_ARRAY);
	glVertexPointer(3, GL_FLOAT, sizeof(Point), &segments->x);
	glDrawArrays(GL_LINES, 0, nb*2);
	glColor4f(1.0f, 1.0f, 1.0f, 1.0f);
    end;
  procedure Make_Triangle ();(const Point& p0, const Point& p1, const Point& p2, const Point& color) is
    begin
	glColor4f(color.x, color.y, color.z, 1.0f);
	Point av3LineEndpoints[] = {p0, p1, p2};
	glEnableClientState(GL_VERTEX_ARRAY);
	glVertexPointer(3, GL_FLOAT, sizeof(Point), &av3LineEndpoints[0].x);
	glColor4f(1.0f, 1.0f, 1.0f, 1.0f);
    end;
  procedure Make_Polygon ();(udword nb_pts, const Point* pts, const Point& normal, const Point& color) is
    begin
    end;
  procedure Make_Sphere ();(float radius, const PR& pose) is
	void DrawSphereWireframe(float radius, const PR& pose, const Point& color)
    begin
	glPushMatrix();
		SetupGLMatrix(pose);
		glScalef(radius, radius, radius);
		glutSolidSphere(1.0f, 12, 12);
	glPopMatrix();
    end;
  procedure Make_Box ();(const Point& extents, const PR& pose) is
    begin
	glPushMatrix();
		SetupGLMatrix(pose);
		glScalef(extents.x, extents.y, extents.z);
		glutSolidCube(2.0f);
	glPopMatrix();
    end;
  procedure Make_Capsule ();(float r, float h, const PR& pose);
	void DrawCapsuleWireframe(float r, float h, const PR& pose, const Point& color) is
    begin
	glPushMatrix();
		glTranslatef(0.0f, h*0.5f, 0.0f);
		glScalef(r, r, r);
		glutSolidSphere(1.0f, 12, 12);  // doesn't include texcoords
	glPopMatrix();

	glPushMatrix();
		glTranslatef(0.0f, -h*0.5f, 0.0f);
		glScalef(r, r, r);
		glutSolidSphere(1.0f, 12, 12);  // doesn't include texcoords
	glPopMatrix();

	glPushMatrix();
		glTranslatef(0.0f, h*0.5f, 0.0f);
		glScalef(r, h, r);
		glRotatef(90.0f, 1.0f, 0.0f, 0.0f);
		DrawCylinder(false);
	glPopMatrix();
    end;
  procedure Make_Cylinder (Radius, Height : Real; Pose : ) is
    begin
glEnableClientState(GL_NORMAL_ARRAY);
  glVertexPointer(3, GL_FLOAT, 2*3*sizeof(float), gCylinderData);
   glNormalPointer(GL_FLOAT, 2*3*sizeof(float), gCylinderData+3);
glDrawArrays(GL_TRIANGLE_STRIP, 0, 13*2);
     if Draw_Caps then
       glVertexPointer(3, GL_FLOAT, 2*3*sizeof(float), gCylinderDataCapsTop);
       glNormalPointer(GL_FLOAT, 2*3*sizeof(float), gCylinderDataCapsTop+3);
       glDrawArrays(GL_TRIANGLES, 0, 36);
       glVertexPointer(3, GL_FLOAT, 2*3*sizeof(float), gCylinderDataCapsBottom);
       glNormalPointer(GL_FLOAT, 2*3*sizeof(float), gCylinderDataCapsBottom+3);
       glDrawArrays(GL_TRIANGLES, 0, 36);
     end if;
    end;
--
--  void DrawCapsuleWireframe(float r, float h, const PR& pose, const Point& color)
--  {
--  	const udword NbSegments = 14;
--
--  	Matrix3x3 M = pose.mRot;
--
--  	Point p0, p1;
--
--  	p1 = M[1];
--  	p1 *= 0.5f*h;
--  	p0 = -p1;
--  	p0 += pose.mPos;
--  	p1 += pose.mPos;
--
--  	Point c0 = M[0];
--  	Point c1 = M[1];
--  	Point c2 = M[2];
--  	DrawLine(p0 + c0*r, p1 + c0*r, color);
--  	DrawLine(p0 - c0*r, p1 - c0*r, color);
--  	DrawLine(p0 + c2*r, p1 + c2*r, color);
--  	DrawLine(p0 - c2*r, p1 - c2*r, color);
--
--  	Matrix4x4 MM;
--  	MM.SetRow(0, -c1);
--  	MM.SetRow(1, -c0);
--  	MM.SetRow(2, c2);
--  	MM.SetTrans(p0);
--  	DrawCircle(NbSegments, MM, color, r, true);	//halfcircle -- flipped
--
--  	MM.SetRow(0, c1);
--  	MM.SetRow(1, -c0);
--  	MM.SetRow(2, c2);
--  	MM.SetTrans(p1);
--  	DrawCircle(NbSegments, MM, color, r, true);
--
--  	MM.SetRow(0, -c1);
--  	MM.SetRow(1, c2);
--  	MM.SetRow(2, c0);
--  	MM.SetTrans(p0);
--  	DrawCircle(NbSegments, MM, color, r, true);//halfcircle -- good
--
--  	MM.SetRow(0, c1);
--  	MM.SetRow(1, c2);
--  	MM.SetRow(2, c0);
--  	MM.SetTrans(p1);
--  	DrawCircle(NbSegments, MM, color, r, true);
--
--  	MM.SetRow(0, c2);
--  	MM.SetRow(1, c0);
--  	MM.SetRow(2, c1);
--  	MM.SetTrans(p0);
--  	DrawCircle(NbSegments, MM, color, r);	//full circle
--  	MM.SetTrans(p1);
--  	DrawCircle(NbSegments, MM, color, r);
--  }
--
--  void DrawCylinder(float r, float h, const PR& pose)
--  {
--  	glPushMatrix();
--  		SetupGLMatrix(pose);
--  		DrawCylinder(r, h);
--  	glPopMatrix();
--  }
--
--  static ConvexHull* CreateConvexHull(udword nb_verts, const Point* verts)
--  {
--  	ConvexHull* CH = ICE_NEW(ConvexHull);
--  	ASSERT(CH);
--
--  	CONVEXHULLCREATE Create;
--  	Create.NbVerts		= nb_verts;
--  	Create.Vertices		= verts;
--  	Create.UnifyNormals	= true;
--  	Create.PolygonData	= true;
--
--  	bool status = CH->Compute(Create);
--  	ASSERT(status);
--  	if(!status)
--  	{
--  		DELETESINGLE(CH);
--  	}
--
--  	return CH;
--  }
--
--  static void DrawHull(const ConvexHull& hull)
--  {
--  	const Point* ConvexVerts = hull.GetVerts();
--  	const udword NbPolys = hull.GetNbPolygons();
--  //	printf("NbPolys: %d\n", NbPolys);
--  	glEnableClientState(GL_VERTEX_ARRAY);
--  	for(udword i=0;i<NbPolys;i++)
--  	{
--  		const HullPolygon& PolygonData = hull.GetPolygon(i);
--  		glNormal3f(PolygonData.mPlane.n.x, PolygonData.mPlane.n.y, PolygonData.mPlane.n.z);
--
--  		const udword NbVertsInPoly = PolygonData.mNbVerts;
--  		const udword NbTris = NbVertsInPoly - 2;
--  		const udword* Indices = PolygonData.mVRef;
--  		udword Offset = 1;
--  		if(1)
--  		{
--  			for(udword i=0;i<NbTris;i++)
--  			{
--  				const udword VRef0 = Indices[0];
--  				const udword VRef1 = Indices[Offset];
--  				const udword VRef2 = Indices[Offset+1];
--  				Offset++;
--
--  				const Point av3LineEndpoints[] = {ConvexVerts[VRef0], ConvexVerts[VRef1], ConvexVerts[VRef2]};
--  				glVertexPointer(3, GL_FLOAT, sizeof(Point), &av3LineEndpoints[0].x);
--  				glDrawArrays(GL_TRIANGLES, 0, 3);
--  			}
--  		}
--  		else
--  		{
--  			Point Vertices[1024];
--  			for(udword i=0;i<NbVertsInPoly;i++)
--  			{
--  				Vertices[i] = ConvexVerts[Indices[i]];
--  			}
--  			glVertexPointer(3, GL_FLOAT, sizeof(Point), &Vertices[0].x);
--  			glDrawArrays(GL_TRIANGLE_FAN, 0, NbVertsInPoly);
--  		}
--  	}
--  	glDisableClientState(GL_VERTEX_ARRAY);
--  }
--
--  static Container* gShapeRenderers = null;
--
--  static PintShapeRenderer* RegisterShapeRenderer(PintShapeRenderer* renderer)
--  {
--  	ASSERT(renderer);
--
--  	if(!gShapeRenderers)
--  		gShapeRenderers = ICE_NEW(Container);
--  	ASSERT(gShapeRenderers);
--
--  	gShapeRenderers->Add(udword(renderer));
--  	return renderer;
--  }
--
--  	class PintDLShapeRenderer : public PintShapeRenderer
--  	{
--  		Matrix4x4	mCached;
--  		PR			mLastPR;
--  		public:
--
--  		PintDLShapeRenderer() : mShadows(true)
--  		{
--  			mDisplayListNum = glGenLists(1);
--  			mLastPR.mPos.SetNotUsed();
--  			mLastPR.mRot.SetNotUsed();
--  		}
--
--  		virtual	~PintDLShapeRenderer()
--  		{
--  			glDeleteLists(mDisplayListNum, 1);
--  		}
--
--  		virtual	void	Render(const PR& pose)
--  		{
--  			glPushMatrix();
--  			{
--  				if(0)
--  				{
--  					if(pose!=mLastPR)
--  					{
--  						mLastPR = pose;
--  						mCached = pose;
--  					}
--
--  					{
--  						const Matrix4x4& M = mCached;
--
--  						float glmat[16];	//4x4 column major matrix for OpenGL.
--  						glmat[0] = M.m[0][0];
--  						glmat[1] = M.m[0][1];
--  						glmat[2] = M.m[0][2];
--  						glmat[3] = M.m[0][3];
--
--  						glmat[4] = M.m[1][0];
--  						glmat[5] = M.m[1][1];
--  						glmat[6] = M.m[1][2];
--  						glmat[7] = M.m[1][3];
--
--  						glmat[8] = M.m[2][0];
--  						glmat[9] = M.m[2][1];
--  						glmat[10] = M.m[2][2];
--  						glmat[11] = M.m[2][3];
--
--  						glmat[12] = M.m[3][0];
--  						glmat[13] = M.m[3][1];
--  						glmat[14] = M.m[3][2];
--  						glmat[15] = M.m[3][3];
--
--  						glMultMatrixf(&(glmat[0]));
--  					//	glLoadMatrixf(&(glmat[0]));
--  					}
--  				}
--  				else
--  				{
--  					SetupGLMatrix(pose);
--  				}
--  //				glColor4f(1.0f, 0.0f, 0.0f, 1.0f);
--  				glCallList(mDisplayListNum);
--  			}
--  			glPopMatrix();
--
--  			if(0 && mShadows)
--  			{
--  				glPushMatrix();
--  				{
--  					const static float shadowMat[]={ 1,0,0,0, 0,0,0,0, 0,0,1,0, 0,0,0,1 };
--  					glMultMatrixf(shadowMat);
--  	//				glMultMatrixf(glMat);
--  					SetupGLMatrix(pose);
--  					glDisable(GL_LIGHTING);
--  					glColor4f(0.1f, 0.2f, 0.3f, 1.0f);
--  					//glutSolidCube(float(size_t(actor->userData))*2.0f);
--  					glCallList(mDisplayListNum);
--  					glEnable(GL_LIGHTING);
--  				}
--  				glPopMatrix();
--  			}
--  		}
--
--  		virtual	void	SetColor(const Point& color, bool isStatic)
--  		{
--  			const float Coeff = isStatic ? 0.5f : 1.0f;
--  			glColor3f(color.x*Coeff, color.y*Coeff, color.z*Coeff);
--  		}
--
--  		virtual	void	SetShadows(bool flag)
--  		{
--  			mShadows = flag;
--  		}
--
--  		GLuint		mDisplayListNum;
--  		bool		mShadows;
--  	};
--
--  	class PintSphereShapeRenderer : public PintDLShapeRenderer
--  	{
--  		public:
--
--  		PintSphereShapeRenderer(float radius)
--  		{
--  			glNewList(mDisplayListNum, GL_COMPILE);
--  				glutSolidSphere(radius, 12, 12);
--  			glEndList();
--  		}
--  	};
--
--  	class PintCapsuleShapeRenderer : public PintDLShapeRenderer
--  	{
--  		public:
--
--  		PintCapsuleShapeRenderer(float r, float h)
--  		{
--  			glNewList(mDisplayListNum, GL_COMPILE);
--  				DrawCapsule(r, h);
--  			glEndList();
--  		}
--  	};
--
--  	class PintCylinderShapeRenderer : public PintDLShapeRenderer
--  	{
--  		public:
--
--  		PintCylinderShapeRenderer(float r, float h)
--  		{
--  			glNewList(mDisplayListNum, GL_COMPILE);
--  				DrawCylinder(r, h);
--  			glEndList();
--  		}
--  	};
--
--  	class PintBoxShapeRenderer : public PintDLShapeRenderer
--  	{
--  		public:
--
--  		PintBoxShapeRenderer(const Point& extents)
--  		{
--  			glNewList(mDisplayListNum, GL_COMPILE);
--  				glScalef(extents.x, extents.y, extents.z);
--  				glutSolidCube(2.0f);
--  			glEndList();
--  		}
--  	};
--
--  	class PintConvexShapeRenderer : public PintDLShapeRenderer
--  	{
--  		public:
--
--  		PintConvexShapeRenderer(udword nb_verts, const Point* verts)
--  		{
--  			ConvexHull* CHull = CreateConvexHull(nb_verts, verts);
--  			ASSERT(CHull);
--  			glNewList(mDisplayListNum, GL_COMPILE);
--  				if(CHull)
--  					DrawHull(*CHull);
--  			glEndList();
--  			DELETESINGLE(CHull);
--  		}
--  	};
--
--  	class PintMeshShapeRenderer : public PintDLShapeRenderer
--  	{
--  		public:
--
--  		PintMeshShapeRenderer(const SurfaceInterface& surface)
--  		{
--  			glNewList(mDisplayListNum, GL_COMPILE);
--
--  				glBegin(GL_TRIANGLES);
--  				for(udword i=0;i<surface.mNbFaces;i++)
--  				{
--  					const udword VRef0 = surface.mDFaces[i*3+0];
--  					const udword VRef1 = surface.mDFaces[i*3+1];
--  					const udword VRef2 = surface.mDFaces[i*3+2];
--  					const Point& p0 = surface.mVerts[VRef0];
--  					const Point& p1 = surface.mVerts[VRef1];
--  					const Point& p2 = surface.mVerts[VRef2];
--  //					const Point Normal = ((p2-p0)^(p1-p0)).Normalize();
--  					const Point Normal = ((p1-p0)^(p2-p0)).Normalize();
--  					glNormal3f(Normal.x, Normal.y, Normal.z);
--  					glVertex3f(p0.x, p0.y, p0.z);
--  					glVertex3f(p1.x, p1.y, p1.z);
--  					glVertex3f(p2.x, p2.y, p2.z);
--  				}
--  				glEnd();
--  			glEndList();
--  		}
--  	};
--  \
