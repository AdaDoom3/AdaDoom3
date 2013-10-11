--
-- BZip2.Decompress - decompression of bzip2 data streams.
--
-- bzip2 compresses files using the Burrows-Wheeler block-sorting text
-- compression algorithm, and Huffman coding. Compression is generally
-- considerably better than that achieved by more conventional
-- LZ77/LZ78-based compressors, and approaches the performance of the
-- PPM family of statistical compressors.
--
-- This Ada code (a) is a reworked translation of a Pascal version (b)
-- by Daniel Mantione of the decompression code of libbzip2 (c)
-- by Julian Seward.
-- Both (a) and (b) refer to (c)'s license which follows here, as in
-- bzip version 1.0.5, from http://www.bzip.org :
--
--***************************************************************************
--  bzip2 and libbzip2, version 1.0.5: A program and library for data
--   compression by Julian Seward
--  Copyright (c) 1996-2007 Julian Seward
--  This program, bzip2, the associated library libbzip2, and all
--   documentation, are copyright (c) 1996-2007 Julian Seward.
--  All rights reserved.
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions
--  are met:
--  * Redistributions of source code must retain the above copyright notice,
--     this list of conditions and the following disclaimer.
--  * The origin of this software must not be misrepresented; you must not
--     claim that you wrote the original software. If you use this software
--     in a product, an acknowledgment in the product documentation would be
--     appreciated but is not required.
--  * Altered source versions must be plainly marked as such, and must not be
--     misrepresented as being the original software.
--  * The name of the author may not be used to endorse or promote products
--     derived from this software without specific prior written permission.
--  THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR
--  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
--  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
--  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
--  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
--  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
--  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
--  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
--  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
--  THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--  PATENTS: To the best of my knowledge, bzip2 and libbzip2 do not use any
--  patented algorithms. However, I do not have the resources to
--  carry out a patent search.
--  Therefore I cannot give any guarantee of the above statement.
--***************************************************************************
--
-- Translated on 20-Oct-2009 by (New) P2Ada v. 15-Nov-2006
-- Rework by G. de Montmollin
--
-- Main difference over the FreePascal version: there is no more pointer
-- arithmetics. The only pointer is tt, for dynamically allocating the biggest
-- decoding array.
-- With the appropriate options, the performance is very close to
-- the bzip2 tool in C: it takes around 7%-11% more time according to data
-- to be decompressed. Add 5% when CRC checking is enabled.
-- These timings are obtained with bunzip.adb compiled on GNAT 2008, Win32,
-- with the -O2 -gnatpn -fpeel-loops -funroll-loops -fweb -frename-registers
-- options, average on several runs (see bz_test.cmd).

with Interfaces;

generic

  input_buffer_size : Integer:= 1024;
  output_buffer_size: Integer:= 4096;

  type Buffer is array(Natural range <>) of Interfaces.Unsigned_8;

  check_CRC: Boolean;
  -- ^ useless if the whole bzip stream is in
  -- another CRC-checked stream, like a Zip archive

  -- Input:
  with procedure Read(buf: out Buffer);
  -- Output:
  with procedure Write(buf: in Buffer);

package BZip2 is

  bad_header_magic,
  bad_block_magic,
  data_error,
  block_crc_check_failed,
  randomized_not_yet_implemented: exception;

  procedure Decompress;

end BZip2;
