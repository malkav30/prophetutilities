/**
This is free and unencumbered software released into the public domain.

Anyone is free to copy, modify, publish, use, compile, sell, or
distribute this software, either in source code form or as a compiled
binary, for any purpose, commercial or non-commercial, and by any
means.

In jurisdictions that recognize copyright laws, the author or authors
of this software dedicate any and all copyright interest in the
software to the public domain. We make this dedication for the benefit
of the public at large and to the detriment of our heirs and
successors. We intend this dedication to be an overt act of
relinquishment in perpetuity of all present and future rights to this
software under copyright law.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

For more information, please refer to <https://unlicense.org>
 */
package com.phi.prophetlibrarian;

import java.util.HashMap;
import java.util.Map;

/**
 * @author Philippe Duval
 *
 *         this enum describes a DSI Prophet synth, and the values needed to
 *         manipulate the associated sysexes
 */
public enum ProphetSynth {
    MODEL_P08(35, "Prophet '08", 446, 439, 384, 16, 184), MODEL_REV2(0x2F, "Prophet REV2", 2346, 1171, 1024, 20, 235);

    public final String MODEL_LITERAL; // the name of the synth. also, the value returned by enum.toString()
    public final int MODEL_NUMBER; // the number of the model, as encoded in the sysex (refer to the documentation
                                   // of the synth to know the number)
    public final int SYSEX_SIZE; // number of bytes used to describe a raw sysex file.
    public final int PROGRAM_SIZE; // number of bytes used to describe a program in raw sysex file. WARN: this is
                                   // for a single layer !!!
    public final int DECODED_PROGRAM_SIZE; // size of a decoded program, once decoded from MSB-encoded format
    public final int PATCH_NAME_SIZE; // length of a patch name in bytes
    public final int PATCH_NAME_START; // offset of the beginning of the patch name data in the sysex

    private static final Map<Integer, ProphetSynth> BY_NUMBER = new HashMap<>();

    static {
        for (ProphetSynth e : values()) {
            BY_NUMBER.put(e.MODEL_NUMBER, e);
        }
    }

    ProphetSynth(int model_sysex, String model_literal, int model_sysex_size, int model_program_size,
            int model_decoded_program_size, int model_name_size, int model_name_start) {
        this.MODEL_NUMBER = model_sysex;
        this.MODEL_LITERAL = model_literal;
        this.SYSEX_SIZE = model_sysex_size;
        this.PROGRAM_SIZE = model_program_size;
        this.DECODED_PROGRAM_SIZE = model_decoded_program_size;
        this.PATCH_NAME_SIZE = model_name_size;
        this.PATCH_NAME_START = model_name_start;
    }

    /**
     * allows to retrieve a synth model by it's sysex number. throws
     * IllegalArgumentException if the synth is not known
     * 
     * @param number
     * @return
     */
    public static ProphetSynth fromSysexNumber(int number) {
        if (BY_NUMBER.get(number) == null) {
            throw new IllegalArgumentException("Synth Model Not Supported !");
        } else {
            return BY_NUMBER.get(number);
        }

    }

    @Override
    public String toString() {
        return this.MODEL_LITERAL;
    }

    // public String literal() {return this.model_literal;}
    // public int sysex() {return this.model_sysex;}
    // public int sysex_size(){return this.model_sysex_size;}
    // public int program_size(){return this.model_program_size;}
    // public int decoded_program_size(){return this.model_decoded_program_size;}
    // public int name_size(){return this.model_name_size;}
    // public int name_start(){return this.model_name_start;}
    public static void main(String[] args) {
        for (ProphetSynth synth : ProphetSynth.values()) {
            System.out.println(ProphetSynth.valueOf("MODEL_P08"));
            System.out.println(synth);
            System.out.println(synth.MODEL_NUMBER);
        }
        System.out.println(ProphetSynth.BY_NUMBER.get(47));
    }
}