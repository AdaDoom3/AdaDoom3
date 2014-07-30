separate(Neo.System.Sound)
package body OpenAL
  is 
  end OpenAL;
-- Oddity007 / gist:965399
-- typedef struct{
	-- ALuint ID;
 
	-- stb_vorbis* stream;
	-- stb_vorbis_info info;
 
	-- ALuint buffers[2];
	-- ALuint source;
	-- ALenum format;
 
	-- size_t bufferSize;
 
	-- size_t totalSamplesLeft;
 
	-- bool shouldLoop;
-- }AudioStream;
 
-- void AudioStreamInit(AudioStream* self){
	-- memset(self, 0, sizeof(AudioStream));
	-- alGenSources(1, & self->source);
	-- alGenBuffers(2, self->buffers);
	-- self->bufferSize=4096*8;
	-- self->shouldLoop=true;//We loop by default
-- }
 
-- void AudioStreamDeinit(AudioStream* self){
	-- alDeleteSources(1, & self->source);
	-- alDeleteBuffers(2, self->buffers);
	-- stb_vorbis_close(self->stream);
	-- memset(self, 0, sizeof(AudioStream));
-- }
 
-- bool AudioStreamStream(AudioStream* self, ALuint buffer){
	-- //Uncomment this to avoid VLAs
	-- //#define BUFFER_SIZE 4096*32
	-- #ifndef BUFFER_SIZE//VLAs ftw
	-- #define BUFFER_SIZE (self->bufferSize)
	-- #endif
	-- ALshort pcm[BUFFER_SIZE];
	-- int  size = 0;
	-- int  result = 0;
 
	-- while(size < BUFFER_SIZE){
		-- result = stb_vorbis_get_samples_short_interleaved(self->stream, self->info.channels, pcm+size, BUFFER_SIZE-size);
		-- if(result > 0) size += result*self->info.channels;
		-- else break;
	-- }
 
	-- if(size == 0) return false;
 
	-- alBufferData(buffer, self->format, pcm, size*sizeof(ALshort), self->info.sample_rate);
	-- self->totalSamplesLeft-=size;
	-- #undef BUFFER_SIZE
 
	-- return true;
-- }
 
-- bool AudioStreamOpen(AudioStream* self, const char* filename){
	-- self->stream = stb_vorbis_open_filename((char*)filename, NULL, NULL);
	-- if(not self->stream) return false;
	-- // Get file info
	-- self->info = stb_vorbis_get_info(self->stream);
	-- if(self->info.channels == 2) self->format = AL_FORMAT_STEREO16;
	-- else self->format = AL_FORMAT_MONO16;
 
	-- if(not AudioStreamStream(self, self->buffers[0])) return false;
	-- if(not AudioStreamStream(self, self->buffers[1])) return false;
	-- alSourceQueueBuffers(self->source, 2, self->buffers);
	-- alSourcePlay(self->source);
 
	-- self->totalSamplesLeft=stb_vorbis_stream_length_in_samples(self->stream) * self->info.channels;
 
	-- return true;
-- }
 
-- bool AudioStreamUpdate(AudioStream* self){
	-- ALint processed=0;
 
    -- alGetSourcei(self->source, AL_BUFFERS_PROCESSED, &processed);
 
    -- while(processed--){
        -- ALuint buffer=0;
        
        -- alSourceUnqueueBuffers(self->source, 1, &buffer);
 
		-- if(not AudioStreamStream(self, buffer)){
			-- bool shouldExit=true;
 
			-- if(self->shouldLoop){
				-- stb_vorbis_seek_start(self->stream);
				-- self->totalSamplesLeft=stb_vorbis_stream_length_in_samples(self->stream) * self->info.channels;
				-- shouldExit=not AudioStreamStream(self, buffer);
			-- }
 
			-- if(shouldExit) return false;
		-- }
		-- alSourceQueueBuffers(self->source, 1, &buffer);
	-- }
 
	-- return true;
-- }
