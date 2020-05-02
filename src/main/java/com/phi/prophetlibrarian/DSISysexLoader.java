/**
 * FIXME licence and doc
 */
package com.phi.prophetlibrarian;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.ByteBuffer;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * @author Philippe Duval
 *
 */
public class DSISysexLoader {

	private static final int CHUNK_SIZE = 8;
	private static final Map<Integer, String> synthBank;
	private static final Map<Integer, String> synthsModels;
	private static final short SYSTEM_EXCLUSIVE = 0xF0;
	private static final short EOX = 0xF7;
	private static final short PROGRAM_DATA = 0b0010;
	private final static char[] hexArray = "0123456789ABCDEF".toCharArray();
	private static final int REV2_PROGRAM_SIZE = 1171; // number of bytes used to describe a program in raw sysex file
	private static final int REV2_DECODED_PROGRAM_SIZE = 1024;
	private static final int REV2_NAME_SIZE = 20;
	private static final int REV2_NAME_START = 235;
	private static final int P08_PROGRAM_SIZE = 439;
	private static final int P08_DECODED_PROGRAM_SIZE = 384;
	private static final int P08_NAME_SIZE = 16;
	private static final int P08_NAME_START = 184;
	private static final int MODEL_P08 = 35;
	private static final int MODEL_REV2 = 0x2F;
	private static final String MODEL_P08_STRING = "Prophet '08";
	private static final String MODEL_REV2_STRING = "Prophet REV2";

	static {
		Map<Integer, String> aMap = new HashMap<>();
		aMap.put(0, "A");
		aMap.put(1, "B");
		aMap.put(2, "C");
		aMap.put(3, "D");
		synthBank = Collections.unmodifiableMap(aMap);

		Map<Integer, String> anotherMap = new HashMap<>();
		anotherMap.put(MODEL_P08, MODEL_P08_STRING);
		anotherMap.put(MODEL_REV2, MODEL_REV2_STRING);
		synthsModels = Collections.unmodifiableMap(anotherMap);
	}

	public static ByteBuffer loadSysexFile(URI uri) throws URISyntaxException, IOException {
		// Load entire file
		ByteBuffer sysex = ByteBuffer.wrap(Files.readAllBytes(Paths.get(uri)));
		if (DSISysexLoader.isPatchData(sysex)) {
			return sysex;
		} else {
			throw new IllegalArgumentException("File is not a program data sysex !");
		}
	}

	public static String toHex(ByteBuffer sysex) {
		char[] hexChars = new char[sysex.limit() * 2];
		for (int j = 0; j < sysex.limit(); j++) {
			int v = sysex.get(j) & 0xFF; // from signed to unsigned
			hexChars[j * 2] = hexArray[v >>> 4];
			hexChars[j * 2 + 1] = hexArray[v & 0x0F];
		}
		return new String(hexChars);
	}

	/**
	 * returns a litteral representation of the program position
	 * 
	 * @param patch
	 * @return the bank and patch number in the REV2 fashion, i.e. B55 or C03
	 */
	public static String getPosition(ByteBuffer patch) {
		// Parse patch to get bank (ABCD) and patch number foo
		int banknumber = Integer.valueOf(patch.get(4));
		String banklitteral = synthBank.get(banknumber);
		// get patchnumber from string
		int patchnumber = Integer.valueOf(patch.get(5));
		return banklitteral + String.format("%03d", patchnumber + 1);// don't forget that index begins at 1 in the synth
	}

	public static String formatSysex(String sysex) {
		return sysex.replaceAll("(.{2})", "$1 ");
	}

	// for Prophet '08, the sysex format is different : 184-199 X 32 - 127 Name
	// charaters 1 – 16, in ASCII format
	// programdata is 439 bytes in “packed MS bit” format (see page 54).
	public static String getP08Name(ByteBuffer patch) {
		// FIXME using bytebuffers is cumbersome here, translate it to bytearrays
		if (!DSISysexLoader.isPatchData(patch)) {
			throw new IllegalArgumentException("sysex is not a program data");
		}
		if (DSISysexLoader.getSynthModel(patch) != MODEL_P08_STRING) {
			throw new IllegalArgumentException("wrong synth model !!!");
		}
		// 1. isolate the program data (index 6 in the sysex payload)
		byte[] programData = new byte[P08_PROGRAM_SIZE];
		patch.rewind();
		patch.position(6);
		patch.get(programData, 0, P08_PROGRAM_SIZE);
		ByteBuffer encodedLayer = ByteBuffer.wrap(programData);
		// 2. decode it chunk by chunk
		byte[] decodedProgramData = new byte[P08_DECODED_PROGRAM_SIZE];
		ByteBuffer decodedLayer = ByteBuffer.wrap(decodedProgramData);
		byte[] chunk = new byte[CHUNK_SIZE];
		for (int i = 0; i < programData.length - CHUNK_SIZE; i += CHUNK_SIZE) {
			// FIXME the last chunk is incomplete, adapt the decode method to deal with it !
			encodedLayer.get(chunk, 0, CHUNK_SIZE); // consume 8 bytes in the buffer --> chunk
			// System.out.println("loop " + i + " / " + programData.length);
			// System.out.println(chunk[0] + " " + chunk[1] + " " + chunk[2] + " " + chunk[3] + " " + chunk[4] + " "
					// + chunk[5] + " " + chunk[6] + " " + chunk[7]);
			decodedLayer.put(DSISysexLoader.unpackData(ByteBuffer.wrap(chunk)));// pass the chunk as a new Bytebuffer,
																				// decode it, and append it to the
																				// decodedlayer
		}
		// FIXME the last chunk is incomplete, adapt the decode method to deal with it !
		// the last chunk should be decoded by hand,

		// 3. isolate the part that we need to get the name(s) (indexes 184-199)
		byte[] layerName = new byte[P08_NAME_SIZE];
		decodedLayer.rewind();
		decodedLayer.position(P08_NAME_START);
		decodedLayer.get(layerName, 0, P08_NAME_SIZE);

		// 4. translate the bytes in string
		String name = "";
		try {
			name = new String(layerName, "US-ASCII");
		} catch (UnsupportedEncodingException e) {
			throw new IllegalArgumentException("Wrong encoding for the name (corrupted sysex ?)", e);
		}
		return name;
	}

	public static String getREV2Name(ByteBuffer patch) {
		if (!DSISysexLoader.isPatchData(patch)) {
			throw new IllegalArgumentException("sysex is not a program data");
		}
		if (DSISysexLoader.getSynthModel(patch) != MODEL_REV2_STRING) {
			throw new IllegalArgumentException("wrong synth model !!!");
		}

		// 1. isolate the program data (index 6 in the sysex payload)
		byte[] programData = new byte[REV2_PROGRAM_SIZE];
		patch.rewind();
		patch.position(6);
		patch.get(programData, 0, REV2_PROGRAM_SIZE);
		ByteBuffer encodedLayer = ByteBuffer.wrap(programData);
		// 2. decode it chunk by chunk
		// FIXME the last chunk is incomplete, adapt the decode method to deal with it !
		byte[] decodedProgramData = new byte[REV2_DECODED_PROGRAM_SIZE];
		ByteBuffer decodedLayer = ByteBuffer.wrap(decodedProgramData);
		byte[] chunk = new byte[CHUNK_SIZE];
		for (int i = 0; i < programData.length - CHUNK_SIZE; i += CHUNK_SIZE) {
			encodedLayer.get(chunk, 0, CHUNK_SIZE); // consume 8 bytes in the buffer --> chunk
			// System.out.println(chunk[0] + " " + chunk[1] + " " + chunk[2] + " " + chunk[3] + " " + chunk[4]);
			decodedLayer.put(DSISysexLoader.unpackData(ByteBuffer.wrap(chunk)));// pass the chunk as a new Bytebuffer,
																				// decode it, and append it to the
																				// decodedlayer
		}

		// 3. isolate the part that we need to get the name(s) (indexes 235-->254)
		byte[] layerName = new byte[REV2_NAME_SIZE];
		decodedLayer.rewind();
		decodedLayer.position(REV2_NAME_START);
		decodedLayer.get(layerName, 0, REV2_NAME_SIZE);

		// 4. translate the bytes in string
		String name = "";
		try {
			name = new String(layerName, "US-ASCII");
		} catch (UnsupportedEncodingException e) {
			throw new IllegalArgumentException("Wrong encoding for the name (corrupted sysex ?)", e);
		}
		return name;
	}

	public static boolean isSysexFile(ByteBuffer sysex) {
		return SYSTEM_EXCLUSIVE == (sysex.get(0) & 0xFF) && EOX == (sysex.get(sysex.limit() - 1) & 0xFF);
	}

	public static String getSynthModel(ByteBuffer sysex) {
		return synthsModels.get(sysex.get(2) & 0xFF);
	}

	public static boolean isPatchData(ByteBuffer sysex) {
		return isSysexFile(sysex) && (PROGRAM_DATA == (sysex.get(3) & 0xFF));
	}

	/**
	 * Unpacks data chunk encoded in DSI format: in DSI sysex format, Data is packed
	 * in 8 byte “packets”, with the MS bit stripped from 7 parameter bytes, and
	 * packed into an eighth byte, which is sent at the start of the 8 byte packet.
	 * Example: Input Data Packed MIDI data 1 A7 A6 A5 A4 A3 A2 A1 A0 1 00 G7 F7 E7
	 * D7 C7 B7 A7 2 B7 B6 B5 B4 B3 B2 B1 B0 2 00 A6 A5 A4 A3 A2 A1 A0 3 C7 C6 C5 C4
	 * C3 C2 C1 C0 3 00 B6 B5 B4 B3 B2 B1 B0 4 D7 D6 D5 D4 D3 D2 D1 D0 4 00 C6 C5 C4
	 * C3 C2 C1 C0 5 E7 E6 E5 E4 E3 E2 E1 E0 5 00 D6 D5 D4 D3 D2 D1 D0 6 F7 F6 F5 F4
	 * F3 F2 F1 F0 6 00 E6 E5 E4 E3 E2 E1 E0 7 G7 G6 G5 G4 G3 G2 G1 G0 7 00 F6 F5 F4
	 * F3 F2 F1 F0 8 00 G6 G5 G4 G3 G2 G1 G0
	 * 
	 * @param buffer the chunk to decode. It should be 8 bytes long, as stated byt
	 *               DSI documentation
	 * @return
	 */
	public static ByteBuffer unpackData(ByteBuffer buffer) {
		// first first, verify that we have 8 bytes, and that each byte begins with 0 as
		// the MSB
		if (buffer.limit() != 8) {
			throw new IllegalArgumentException("Size of chunk is not 8 byte");
		}
		// first, read the first byte to reconstruct the MSBs
		byte msb = buffer.get(0);

		ByteBuffer unpackedBuffer = ByteBuffer.allocate(7);
		for (int i = 1; i < buffer.limit(); i++) {
			// try to reconstruct A : A is A7 (LSB of MSB) then 2nd byte
			// A7 is obtained by msb AND 0b00000001 which is 2^(i-1) (next mask should be 2,
			// then 4, then 8...)
			int _A7 = msb & (int) (Math.pow(2, i - 1)); // (2^(i-1))
			// make it rotate to have the MSB right again, the amount of rotation being 8
			// minus the number of the byte to reconstruct
			// e.g to reconstruct C the 3rd byte, we will have to shift the MSB 5 times to
			// the left, thus 8-3
			_A7 = _A7 << (8 - i);
			// Now we can make a OR to get back our first byte
			int _A = _A7 | buffer.get(i);
			unpackedBuffer.put(i - 1, (byte) _A);

		}
		return unpackedBuffer;
	}

	public static String getFullName(ByteBuffer patch) {
		// get position
		//System.out.println(DSISysexLoader.toHex(patch));
		String pos = DSISysexLoader.getPosition(patch);
		// System.out.println(pos);
		// get name
		String name = "";
		switch (DSISysexLoader.getSynthModel(patch)) {
			case MODEL_P08_STRING:
				name = DSISysexLoader.getP08Name(patch);
				break;
			case MODEL_REV2_STRING:
				name = DSISysexLoader.getREV2Name(patch);
				break;
			default:
				throw new IllegalArgumentException("Wrong Synth Model");
		}

		// concat, trim, return
		// System.out.println(pos.concat(" - ").concat(name).trim());
		return pos.concat(" - ").concat(name).trim();
	}

	public static void main(String[] args) {
		// this should take only 1 arg
		if (args.length != 1) {
			usage();
			System.exit(-1);
		}
		try {
			URI patchPath = Paths.get("",args).toAbsolutePath().normalize().toUri();
			ByteBuffer patch = DSISysexLoader.loadSysexFile(patchPath);
			System.out.println("Patch name is "+DSISysexLoader.getFullName(patch));
		} catch (MalformedURLException e) {
			System.out.println("Problem locating the indicated file, check your syntax !");
			e.printStackTrace();
			System.exit(-1);
		} catch (URISyntaxException e) {
			System.out.println("Error parsing the indicated file path, check your syntax !");
			e.printStackTrace();
			System.exit(-1);
		} catch (IOException e) {
			System.out.println("Unable to read sysex patch; error is:");
			e.printStackTrace();
			System.exit(-1);
		}
		System.exit(0);
	}

	private static void usage() {
		System.out.println("Prophet name retriever - use it to retrieve the name and position of a patch\n");
		System.out.println("Usage:\n");
		System.out.println("	java -jar prophet.jar <sysexfile.syx>");
	}
}
