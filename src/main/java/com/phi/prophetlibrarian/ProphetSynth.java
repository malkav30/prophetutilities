package com.phi.prophetlibrarian;

import java.util.HashMap;
import java.util.Map;

public enum ProphetSynth {
    MODEL_P08(35,"Prophet '08",446,439,384,16,184),
    MODEL_REV2(0x2F,"Prophet REV2",2346,1171,1024,20,235);

    public final String model_literal;
    public final int model_number;
    public final int model_sysex_size;
    public final int model_program_size;
    public final int model_decoded_program_size;
    public final int model_name_size;
    public final int model_name_start;
    
    private static final Map<Integer, ProphetSynth> BY_NUMBER = new HashMap<>();
         
    static {
        for (ProphetSynth e : values()) {
            BY_NUMBER.put(e.model_number, e);

        }
    }

    ProphetSynth(int model_sysex, String model_literal, int model_sysex_size, int model_program_size, int model_decoded_program_size, int model_name_size, int model_name_start){
        this.model_number=model_sysex;
        this.model_literal=model_literal;
        this.model_sysex_size=model_sysex_size;
        this.model_program_size=model_program_size;
        this.model_decoded_program_size=model_decoded_program_size;
        this.model_name_size=model_name_size;
        this.model_name_start=model_name_start;
    }

    public static ProphetSynth valueOfSysexNumber(int number) {
        return BY_NUMBER.get(number);
    }

    @Override
    public String toString() {
        return this.model_literal;
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
            System.out.println(synth.model_number);
        }
        System.out.println(ProphetSynth.BY_NUMBER.get(47));
    }
}