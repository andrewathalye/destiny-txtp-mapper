#define POSIXLY_CORRECT
#include <stdio.h>
#include <stdlib.h>

#include "vgmstream.h"
#include "util.h"
#include "plugins.h"
#define SAMPLE_BUFFER_SIZE 32768

static void make_smpl_chunk(uint8_t* buf, int32_t loop_start, int32_t loop_end) {
    int i;

    memcpy(buf+0, "smpl", 0x04); /* header */
    put_s32le(buf+0x04, 0x3c); /* size */

    for (i = 0; i < 7; i++)
        put_s32le(buf+0x08 + i * 0x04, 0);

    put_s32le(buf+0x24, 1);

    for (i = 0; i < 3; i++)
        put_s32le(buf+0x28 + i * 0x04, 0);

    put_s32le(buf+0x34, loop_start);
    put_s32le(buf+0x38, loop_end);
    put_s32le(buf+0x3C, 0);
    put_s32le(buf+0x40, 0);
}

/* make a RIFF header for .wav */
static size_t make_wav_header(uint8_t* buf, size_t buf_size, int32_t sample_count, int32_t sample_rate, int channels, int smpl_chunk, int32_t loop_start, int32_t loop_end) {
    size_t data_size, header_size;

    data_size = sample_count * channels * sizeof(sample_t);
    header_size = 0x2c;
    if (smpl_chunk && loop_end)
        header_size += 0x3c+ 0x08;

    if (header_size > buf_size)
        goto fail;

    memcpy(buf+0x00, "RIFF", 0x04); /* RIFF header */
    put_32bitLE(buf+0x04, (int32_t)(header_size - 0x08 + data_size)); /* size of RIFF */

    memcpy(buf+0x08, "WAVE", 4); /* WAVE header */

    memcpy(buf+0x0c, "fmt ", 0x04); /* WAVE fmt chunk */
    put_s32le(buf+0x10, 0x10); /* size of WAVE fmt chunk */
    put_s16le(buf+0x14, 0x0001); /* codec PCM */
    put_s16le(buf+0x16, channels); /* channel count */
    put_s32le(buf+0x18, sample_rate); /* sample rate */
    put_s32le(buf+0x1c, sample_rate * channels * sizeof(sample_t)); /* bytes per second */
    put_s16le(buf+0x20, (int16_t)(channels * sizeof(sample_t))); /* block align */
    put_s16le(buf+0x22, sizeof(sample_t) * 8); /* significant bits per sample */

    if (smpl_chunk && loop_end) {
        make_smpl_chunk(buf+0x24, loop_start, loop_end);
        memcpy(buf+0x24+0x3c+0x08, "data", 0x04); /* WAVE data chunk */
        put_u32le(buf+0x28+0x3c+0x08, (int32_t)data_size); /* size of WAVE data chunk */
    }
    else {
        memcpy(buf+0x24, "data", 0x04); /* WAVE data chunk */
        put_s32le(buf+0x28, (int32_t)data_size); /* size of WAVE data chunk */
    }

    /* could try to add channel_layout, but would need to write WAVEFORMATEXTENSIBLE (maybe only if arg flag?) */

    return header_size;
fail:
    return 0;
}

int export_wav (char * name, VGMSTREAM* vgmstream) {
    FILE* outfile = NULL;
    int32_t len_samples;
    sample_t* buf = NULL;
    int i;
    int channels, input_channels;

    channels = vgmstream->channels;
    input_channels = vgmstream->channels;

    vgmstream_mixing_enable(vgmstream, 0, &input_channels, &channels);

    /* last init */
    buf = malloc(SAMPLE_BUFFER_SIZE * sizeof(sample_t) * input_channels);
    if (!buf) {
        fprintf(stderr, "failed allocating output buffer\n");
        goto fail;
    }

    /* simulate seek */
    len_samples = vgmstream_get_samples(vgmstream);

    outfile = fopen(name, "wb");
    if (!outfile) {
    	fprintf(stderr, "failed to open %s for output\n", name);
        goto fail;
    }

    /* slap on a .wav header */
    uint8_t wav_buf[0x100];
    size_t bytes_done;

    bytes_done = make_wav_header(wav_buf, 0x100, len_samples, vgmstream->sample_rate, channels, 0, vgmstream->loop_start_sample, vgmstream->loop_end_sample);

    fwrite(wav_buf, sizeof(uint8_t), bytes_done, outfile);

    /* decode */
    for (i = 0; i < len_samples; i += SAMPLE_BUFFER_SIZE) {
        int to_get = SAMPLE_BUFFER_SIZE;
        if (i + SAMPLE_BUFFER_SIZE > len_samples)
            to_get = len_samples - i;

        render_vgmstream(buf, to_get, vgmstream);

        swap_samples_le(buf, channels * to_get); /* write PC endian */
        fwrite(buf, sizeof(sample_t) * channels, to_get, outfile);
    }

    if (outfile)
        fclose(outfile);
    free(buf);
    return 1;
fail:
    if (outfile)
        fclose(outfile);
    free(buf);
    return 0;
}
