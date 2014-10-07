
  static int x_pos = 0;
  static int y_pos = 0;

  SDLMod km = SDL_GetModState ();

  // Right button
  if (SDL_GetMouseState (NULL, NULL) & SDL_BUTTON_RMASK)
    {
      // Zoom
      eye._z += (x - x_pos) * 0.1f;
    }
  // Left button
  else if (SDL_GetMouseState (NULL, NULL) & SDL_BUTTON_LMASK)
    {
      if ((km & KMOD_RSHIFT) || (km & KMOD_LSHIFT))
	{
	  // Translation
	  eye._x -= (x - x_pos) * 0.1f;
	  eye._y += (y - y_pos) * 0.1f;
	}
      else
	{
	  // Rotation
	  rot._x += (y - y_pos);
	  rot._y += (x - x_pos);
	}
    }

  x_pos = x;
  y_pos = y;
