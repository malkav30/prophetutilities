/**
 * 
 */
package com.phi.prophetlibrarian;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.net.URI;
import java.net.URISyntaxException;
import java.nio.ByteBuffer;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * @author A501341
 *
 */
public class DSISysexLoaderTest {

	// Get file from resources folder
	private URI getFileURI(String name) throws URISyntaxException {
		ClassLoader classLoader = this.getClass().getClassLoader();
		URI uri = classLoader.getResource(name).toURI();
		System.out.println("Loading file: " + uri);

		// Path currentDir = Paths.get(".");
		// System.out.println(currentDir.toAbsolutePath());
		return uri;
	}

	// load hexadecimal sysex file
	@Test
	public void should_load_prophet_instrument_sysex() throws Exception {
		// GIVEN
		URI uri = getFileURI("B2_P01_Strange_Bass.syx");

		// WHEN
		ByteBuffer result = DSISysexLoader.loadSysexFile(uri);
		// THEN
		// System.out.println(DSISysexLoader.formatSysex(DSISysexLoader.toHex(result)));
		Assertions.assertTrue(result.hasArray()); // our buffer is not empty
		Assertions.assertTrue((result.get(0) & 0xFF) == 0xF0); // it begins with F0
	}

	// output the patch number in litteral form
	@ParameterizedTest
	@CsvSource({
		"B2_P01_Strange_Bass.syx,B001",
		"B2 P55 Alpha Lead.syx,B055",
		"Stranger_Things.syx,B108"
	})
	public void should_return_position(String file, String position) throws Exception {
		// GIVEN a loader, and some variables
		URI uri = getFileURI(file);
		ByteBuffer patch = DSISysexLoader.loadSysexFile(uri);

		// WHEN we try to get the position of a sysex file
		String result = DSISysexLoader.getPosition(patch);
		// System.out.println(result);
		// THEN whe should print the position of the patch
		Assertions.assertEquals(result, position);
	}
	/*@Test
	public void should_return_position() throws Exception {
		// GIVEN a loader, and some variables
		URL url = getFileURL("B2 P55 Alpha Lead.syx");
		ByteBuffer patch = DSISysexLoader.loadSysexFile(url);

		// WHEN we try to get the position of a sysex file
		String result = DSISysexLoader.getPosition(patch);
		System.out.println(result);
		// THEN whe should print the position of the patch
		Assertions.assertEquals(result, "B55");
	}*/

	// load a sysex file with several banks, havig e.g. Wagnerian in
	// A1, and Percy in B96 (not tested)
	@Test
	public void should_load_prophet_bank_sysex() throws Exception {
		// GIVEN
		URI uri = getFileURI("Prophet_08_Programs_v1.0.syx");
		// WHEN
		ByteBuffer result = DSISysexLoader.loadSysexFile(uri);
		// THEN
		// System.out.println(DSISysexLoader.formatSysex(DSISysexLoader.toHex(result)));
		Assertions.assertTrue(result.hasArray());
	}

	// convert byte into readable hex
	@Test
	public void should_convert_binary_sysex_to_hex() throws Exception {
		// GIVEN
		byte[] bytes = { 10, 2, 15, 11 };
		ByteBuffer sysex = ByteBuffer.wrap(bytes);
		// System.out.println(sysex);
		// WHEN
		String result = DSISysexLoader.toHex(sysex);
		// THEN
		// System.out.println(DSISysexLoader.formatSysex(result));
		Assertions.assertTrue("0A020F0B".equals(result));
	}

	// get the patch name (not trimmed)
	@Test
	public void should_return_patch_name_strange_bass() throws Exception {
		// GIVEN
		URI uri = getFileURI("B2_P01_Strange_Bass.syx");
		ByteBuffer patch = DSISysexLoader.loadSysexFile(uri);

		// WHEN
		String result = DSISysexLoader.getP08Name(patch);
		// THEN
		assertTrue("Strange Bass JR ".equals(result));
	}

	// get the patch name (not trimmed)
	@Test
	public void should_return_patch_name_alpha_lead() throws Exception {
		// GIVEN
		URI uri = getFileURI("B2 P55 Alpha Lead.syx");
		ByteBuffer patch = DSISysexLoader.loadSysexFile(uri);

		// WHEN
		String result = DSISysexLoader.getP08Name(patch);
		// THEN
		assertTrue("Alpha Lead JR   ".equals(result));
	}

	// get the patch name (not trimmed) for a rev2 patch
	@Test
	public void should_return_patch_name_rev2() throws Exception {
		// GIVEN 
		URI uri = getFileURI("Stranger_Things.syx");
		ByteBuffer patch = DSISysexLoader.loadSysexFile(uri);

		// WHEN
		String result = DSISysexLoader.getREV2Name(patch);
		// System.out.println(result);
		// THEN
		assertTrue("StrangerThings      ".equals(result));
	}

	// ensure that we dont load garbage files not being sysex
	@Test
	public void should_not_validate_notSysex_files() {
		// GIVEN
		byte[] bytes = { 10, 2, 15, 11 };
		ByteBuffer wrongFile = ByteBuffer.wrap(bytes);
		// WHEN / THEN
		assertFalse(DSISysexLoader.isSysexFile(wrongFile));

	}

	// ensure that the patch is a regular sysex file
	@Test
	public void should_validate_sysex_files() throws Exception {
		// GIVEN
		URI uri = getFileURI("B2_P01_Strange_Bass.syx");
		ByteBuffer patch = DSISysexLoader.loadSysexFile(uri);
		// THEN
		assertTrue(DSISysexLoader.isSysexFile(patch));

	}

	// get the prophet revision
	@Test
	public void should_identify_synth() throws Exception {
		// GIVEN
		URI url = getFileURI("B2_P01_Strange_Bass.syx");
		ByteBuffer patch = DSISysexLoader.loadSysexFile(url);
		// WHEN/THEN
		assertTrue("Prophet '08".equals(DSISysexLoader.getSynthModel(patch)));
	}

	// validate that this is a patch sysex, and not some other sysex file
	@Test
	public void should_recognize_patchdata() throws Exception {
		// GIVEN
		URI uri = getFileURI("B2_P01_Strange_Bass.syx");
		ByteBuffer patch = DSISysexLoader.loadSysexFile(uri);
		// WHEN/THEN
		assertTrue((DSISysexLoader.isPatchData(patch)));
	}

	// ensure that we don't load garbage binary files
	@Test
	@SuppressWarnings("unused")
	public void should_not_load_illegal_files() throws URISyntaxException {
		// GIVEN
		URI uri = getFileURI("foo.syx");
		// WHEN
		try {
			ByteBuffer patch = DSISysexLoader.loadSysexFile(uri);
			fail();
		} catch (Exception e) {
			// THEN
			assertTrue(e instanceof IllegalArgumentException);
		}
	}

	// ability to decode MSB encoded binary data
	@Test
	public void should_unpack_chunk() throws Exception {

		/*
		 * let's try to unpack a chunck Unpacks data chunk encoded in DSI format: in DSI
		 * sysex format, Data is packed in 8 byte “packets”, with the MS bit stripped
		 * from 7 parameter bytes, and packed into an eighth byte, which is sent at the
		 * start of the 8 byte packet. Example: Input Data Packed MIDI data 1 A7 A6 A5
		 * A4 A3 A2 A1 A0 1 00 G7 F7 E7 D7 C7 B7 A7 2 B7 B6 B5 B4 B3 B2 B1 B0 2 00 A6 A5
		 * A4 A3 A2 A1 A0 3 C7 C6 C5 C4 C3 C2 C1 C0 3 00 B6 B5 B4 B3 B2 B1 B0 4 D7 D6 D5
		 * D4 D3 D2 D1 D0 4 00 C6 C5 C4 C3 C2 C1 C0 5 E7 E6 E5 E4 E3 E2 E1 E0 5 00 D6 D5
		 * D4 D3 D2 D1 D0 6 F7 F6 F5 F4 F3 F2 F1 F0 6 00 E6 E5 E4 E3 E2 E1 E0 7 G7 G6 G5
		 * G4 G3 G2 G1 G0 7 00 F6 F5 F4 F3 F2 F1 F0 8 00 G6 G5 G4 G3 G2 G1 G0
		 */

		// GIVEN
		byte[] unpackedBytes = { 1, 2, 4, 8, 16, 32, 42 };
		/*
		 * this gives us 1 00000001 2 00000010 3 00000100 4 00001000 5 00010000 6
		 * 00100000 7 00101010
		 */

		byte[] packedBytes = { 0b00000000, 0b00000001, 0b00000010, 0b00000100, 0b00001000, 0b00010000, 0b00100000,
				0b00101010, };
		/*
		 * this gives us 1 00000000 2 00000001 3 00000010 4 00000100 5 00001000 6
		 * 00010000 7 00100000 8 00101010
		 */

		ByteBuffer unpacked = DSISysexLoader.unpackData(ByteBuffer.wrap(packedBytes));
		for (int i = 0; i < 7; i++) {
			assertEquals(unpacked.get(i), unpackedBytes[i]);
		}
	}

	// ability to decode binary encoded data, when it is in unsigned byte format
	@Test
	public void should_unpack_unsigned_chunk() throws Exception {
		/*
		 * In Java all the primitive types, except for char, are signed. You can't have
		 * an unsigned byte. The only thing you may do is to cast the unsigned byte to
		 * an int so you can read its proper value: int a = b & 0xff If you want to
		 * store an unsigned byte value in a byte type, you obviously can, but every
		 * time you need to "process" it, just remember to cast it again as showed
		 * above.
		 */

		/*
		 * Pour les nombres signés, en complément à 2, il faut toujours faire inversion,
		 * puis ajouter 1 : 1110 0011 on inverse => 0001 1100 on ajoute 1 ==> 0001 1101
		 * = 29
		 */
		// GIVEN
		byte[] unpackedBytes = { 1, 2, 4, 8, 16, 32, -29 };
		/*
		 * this gives us 1 00000001 2 00000010 3 00000100 4 00001000 5 00010000 6
		 * 00100000 7 11100011 //here a value > 127, because why not
		 */

		byte[] packedBytes = { 0b01000000, 0b00000001, 0b00000010, 0b00000100, 0b00001000, 0b00010000, 0b00100000,
				0b01100011, };
		/*
		 * this gives us 1 01000000 2 00000001 3 00000010 4 00000100 5 00001000 6
		 * 00010000 7 00100000 8 01100011
		 */

		ByteBuffer unpacked = DSISysexLoader.unpackData(ByteBuffer.wrap(packedBytes));
		for (int i = 0; i < 7; i++) {
			assertEquals(unpackedBytes[i], unpacked.get(i));
		}
	}

	// validate that we don't try to decode a chunk that is bigger than 8 bytes
	@Test
	public void should_not_unpack_toobig_chunk() throws Exception {
		// GIVEN a 9bytes chunk
		byte[] bytes = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
		ByteBuffer illegalChunk = ByteBuffer.wrap(bytes);
		// WHEN
		try {
			DSISysexLoader.unpackData(illegalChunk);
			fail();
		}
		// THEN
		catch (Exception e) {
			assertTrue(e instanceof IllegalArgumentException);
		}
	}

	// validate that we don't try to decode a chunk that is smaller than 8 bytes
	@Test
	public void should_not_unpack_toolittle_chunk() throws Exception {
		// GIVEN a 9bytes chunk
		byte[] bytes = { 0, 1, 2 };
		ByteBuffer illegalChunk = ByteBuffer.wrap(bytes);
		// WHEN
		try {
			DSISysexLoader.unpackData(illegalChunk);
			fail();
		}
		// THEN
		catch (Exception e) {
			assertTrue(e instanceof IllegalArgumentException);
		}
	}

	// finally, validate that we can have the full name, trimmed, including bank/patch number (what we are looking for :) )
	@Test
	public void should_return_full_patch_name_bass() throws Exception {
		//GIVEN a P'08 file
		URI uri = getFileURI("B2_P01_Strange_Bass.syx");
		ByteBuffer patch = DSISysexLoader.loadSysexFile(uri);
		//WHEN we call the method
		String fullName = DSISysexLoader.getFullName(patch);
		//THEN it should return the vull patch name and position
		assertTrue("B001 - Strange Bass JR".equals(fullName));
	}

	// finally, validate that we can have the full name, trimmed, including bank/patch number (what we are looking for :) )
	@Test
	public void should_return_full_patch_name_lead() throws Exception {
		//GIVEN a P'08 file
		URI uri = getFileURI("B2 P55 Alpha Lead.syx");
		ByteBuffer patch = DSISysexLoader.loadSysexFile(uri);
		//WHEN we call the method
		String fullName = DSISysexLoader.getFullName(patch);
		//THEN it should return the vull patch name and position
		assertEquals(fullName,"B055 - Alpha Lead JR"); //0x01 0x36
	}

	// finally, validate that we can have the full name, trimmed, including bank/patch number (what we are looking for :) )
	@Test
	public void should_return_full_patch_name_rev2() throws Exception {
		//GIVEN a REV2 file
		URI uri = getFileURI("Stranger_Things.syx");
		ByteBuffer patch = DSISysexLoader.loadSysexFile(uri);
		//WHEN we call the method
		String fullName = DSISysexLoader.getFullName(patch);
		//THEN it should return the vull patch name and position
		assertEquals(fullName,"B108 - StrangerThings"); //01 6B
	}
}
