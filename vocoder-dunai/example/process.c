
#include <jack/jack.h>
#include <stdint.h>
#include <string.h>

struct portdata {
    jack_port_t *port;
    float *buf;
};

struct vocoder_data {
    intptr_t num;
    void (*tick)();
    struct portdata oport;
    struct portdata iports[];
};

int vocoder_process(jack_nframes_t nframes, struct vocoder_data *arg)
{
    void *buf = jack_port_get_buffer(arg->oport.port, nframes);
    memcpy(buf, arg->oport.buf, sizeof(float) * nframes);
    for (int i = 0; i < arg->num; i++) {
        buf = jack_port_get_buffer(arg->iports[i].port, nframes);
        memcpy(arg->iports[i].buf, buf, sizeof(float) * nframes);
    }
    arg->tick();
    return 0;
}

