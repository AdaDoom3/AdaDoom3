--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
package Neo.File.Audio
  is
  -----------------
  -- Enumeration --
  -----------------
    type Enumerated_Format
      is(
      Wave_Format,
      --
      -- 1991 Microsoft
      --      http://web.archive.org/web/20120531013808/http://www-mmsp.ece.mcgill.ca/documents/audioformats/wave/wave.html
      --
      OGG_Vorbis_Format);
      --
      -- 2001 Xiph.Org Foundation
      --      http://web.archive.org/web/20130728162612/http://xiph.org/vorbis/doc/Vorbis_I_spec.html
      --
    type Enumerated_Sample_Rate
      is(
      Forty_Four_Kilohertz_Rate,
      Twenty_Two_Kilohertz_Rate,
      Eleven_Kilohertz_Rate);
  -------------
  -- Records --
  -------------
    type Record_Playback(
      Length    : Integer_4_Positive;
      Is_Stereo : Boolean := True)
      is record
        Sample_Rate : Enumerated_Sample_Rate := Forty_Four_Kilohertz_Rate;
        case Is_Stereo is
          when True  =>
            Stereo : Array_1x2_Float_4_Real(1..2, 1..Length) := (others => (others => 0.0));
          when False =>
            Mono : Array_Float_4_Real(1..Length) := (others => 0.0);
        end case;
      end record;
  -----------------
  -- Subprograms --
  -----------------
    function Load(
      Path : in String_2)
      return Record_Playback;
    function Save(
-------
private
-------
  end Neo.File.Audio;
