/**
 * 
 */
package com.phi.prophetlibrarian;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.net.URI;
import java.net.URISyntaxException;
import java.nio.ByteBuffer;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Philippe Duval
 *
 */
public class DSISysexLoaderTest {

	private static Logger console = LoggerFactory.getLogger( DSISysexLoaderTest.class );

	// Get file from resources folder
	private URI getFileURI(String name) throws URISyntaxException {
		ClassLoader classLoader = this.getClass().getClassLoader();
		URI uri = classLoader.getResource(name).toURI();
		console.debug("Loading file: {}",uri);
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
		assertThat(result.hasArray()).isEqualTo(true);
		assertThat((result.get(0) & 0xFF)).isEqualTo(0xF0);
	}

	// output the patch number in literal form
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
		// THEN we should print the position of the patch
		//Assertions.assertEquals(result, position);
		assertThat(result).isEqualTo(position);
	}

	// load a sysex file with several banks, having e.g. Wagnerian in
	// A1, and Percy in B96 (not tested)
	@Test
	public void should_load_prophet_bank_sysex() throws Exception {
		// GIVEN
		URI uri = getFileURI("Prophet_08_Programs_v1.0.syx");
		// WHEN
		ByteBuffer result = DSISysexLoader.loadSysexFile(uri);
		// THEN
		assertThat(result.hasArray()).isEqualTo(true);
	}

	// convert byte into readable hex
	@Test
	public void should_convert_binary_sysex_to_hex() throws Exception {
		// GIVEN
		byte[] bytes = { 10, 2, 15, 11 };
		ByteBuffer sysex = ByteBuffer.wrap(bytes);
		// WHEN
		String result = DSISysexLoader.toHex(sysex);
		// THEN
		assertThat(result).isEqualTo("0A020F0B");
	}

	// get the patch name (not trimmed)
	@ParameterizedTest
	@CsvSource({
		"B2_P01_Strange_Bass.syx,'Strange Bass JR '",
		"B2 P55 Alpha Lead.syx,'Alpha Lead JR   '"
	})
	public void should_return_patch_name_p08(String file, String name) throws Exception {
		// GIVEN
		URI uri = getFileURI(file);
		ByteBuffer patch = DSISysexLoader.loadSysexFile(uri);
		// WHEN
		String result = DSISysexLoader.getP08Name(patch);
		// THEN
		assertThat(result).isEqualTo(name);
	}

	// get the patch name (not trimmed) for a rev2 patch
	@ParameterizedTest
	@CsvSource({
		"Stranger_Things.syx,'StrangerThings      '"
	})
	public void should_return_patch_name_rev2(String file, String name) throws Exception {
		// GIVEN 
		URI uri = getFileURI(file);
		ByteBuffer patch = DSISysexLoader.loadSysexFile(uri);

		// WHEN
		String result = DSISysexLoader.getREV2Name(patch);
		// System.out.println(result);
		// THEN
		assertThat(result).isEqualTo(name);
	}

	// ensure that we don't load garbage files not being sysex
	@Test
	public void should_not_validate_notSysex_files() {
		// GIVEN
		byte[] bytes = { 10, 2, 15, 11 };
		ByteBuffer wrongFile = ByteBuffer.wrap(bytes);
		// WHEN / THEN
		assertThat(DSISysexLoader.isSysexFile(wrongFile)).isFalse();

	}

	// ensure that the patch is a regular sysex file
	@Test
	public void should_validate_sysex_files() throws Exception {
		// GIVEN
		URI uri = getFileURI("B2_P01_Strange_Bass.syx");
		ByteBuffer patch = DSISysexLoader.loadSysexFile(uri);
		// THEN
		assertThat(DSISysexLoader.isSysexFile(patch)).isTrue();

	}

	// get the prophet revision
	@ParameterizedTest
	@CsvSource({
		"B2_P01_Strange_Bass.syx,Prophet '08",
		"B2 P55 Alpha Lead.syx,Prophet '08",
		"Stranger_Things.syx,Prophet REV2"
	})
	public void should_identify_synth(String file, String synth) throws Exception {
		// GIVEN
		URI url = getFileURI(file);
		ByteBuffer patch = DSISysexLoader.loadSysexFile(url);
		// WHEN/THEN
		assertThat(synth).isEqualTo(DSISysexLoader.getSynthModel(patch).model_literal);
	}
	
	@ParameterizedTest
	@CsvSource({
	 	"wrongbank.syx"
	})
	public void should_throw_exception_when_wrong_synth(String file) throws Exception {
		// GIVEN
		URI url = getFileURI(file);
		ByteBuffer patch = DSISysexLoader.loadSysexFile(url);
		// WHEN/THEN
		assertThatExceptionOfType(IllegalArgumentException.class).isThrownBy(() -> DSISysexLoader.getSynthModel(patch));
	}

	// validate that this is a patch sysex, and not some other sysex file
	@Test
	public void should_recognize_patchdata() throws Exception {
		// GIVEN
		URI uri = getFileURI("B2_P01_Strange_Bass.syx");
		ByteBuffer patch = DSISysexLoader.loadSysexFile(uri);
		// WHEN/THEN
		assertThat((DSISysexLoader.isPatchData(patch))).isTrue();
	}

	// ensure that we don't load garbage binary files
	@Test
	public void should_not_load_illegal_files() throws URISyntaxException {
		// GIVEN
		URI uri = getFileURI("foo.syx");
		// WHEN
		assertThatExceptionOfType(IllegalArgumentException.class).isThrownBy(() ->
											DSISysexLoader.loadSysexFile(uri));
		
	}

	// ability to decode MSB encoded binary data
	@Test
	public void should_unpack_chunk() throws Exception {

		/*
		 * let's try to unpack a chunk Unpacks data chunk encoded in DSI format: in DSI
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
			assertThat(unpacked.get(i)).isEqualTo(unpackedBytes[i]);
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
			assertThat(unpackedBytes[i]).isEqualTo(unpacked.get(i));
		}
	}

	// validate that we don't try to decode a chunk that is bigger than 8 bytes
	@Test
	public void should_not_unpack_toobig_chunk() throws Exception {
		// GIVEN a 9bytes chunk
		byte[] bytes = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
		ByteBuffer illegalChunk = ByteBuffer.wrap(bytes);
		// WHEN
		// THEN JUnit5 Style
		assertThrows(IllegalArgumentException.class, () -> DSISysexLoader.unpackData(illegalChunk));
	}

	// validate that we don't try to decode a chunk that is smaller than 8 bytes
	@Test
	public void should_not_unpack_toolittle_chunk() throws Exception {
		// GIVEN a 9bytes chunk
		byte[] bytes = { 0, 1, 2 };
		ByteBuffer illegalChunk = ByteBuffer.wrap(bytes);
		// WHEN
		// THEN AssertJ Style
		assertThatExceptionOfType(IllegalArgumentException.class).isThrownBy(() -> DSISysexLoader.unpackData(illegalChunk));
	}


	@ParameterizedTest
	@CsvSource({
	 	"B2_P01_Strange_Bass.syx,1",
	 	"Prophet_08_Programs_v1.0.syx,256",
	 	"Rev2_Programs_v1.0.syx,512"
	})
	public void should_return_patch_list(String file, int patchNumbers) throws Exception {
		//GIVEN
		URI uri = getFileURI(file);
		ByteBuffer sysex = DSISysexLoader.loadSysexFile(uri);
		List<ByteBuffer> patches = DSISysexLoader.getBankNames(sysex);
		//WHEN/THEN
		assertThat(patches).hasSize(patchNumbers);
	}
	
	@ParameterizedTest
	@CsvSource({
	 	"wrongbank.syx"
	})
	public void should_not_return_patch_list_unsupported_synth(String file) throws Exception {
		//GIVEN
		URI uri = getFileURI(file);
		ByteBuffer sysex = DSISysexLoader.loadSysexFile(uri);
		//WHEN/THEN
		assertThatExceptionOfType(IllegalArgumentException.class).isThrownBy(() -> DSISysexLoader.getBankNames(sysex));
	}
	
	// finally, validate that we can have the full name, trimmed, including bank/patch number (what we are looking for :) )
	@ParameterizedTest
	@CsvSource({
		"B2_P01_Strange_Bass.syx,B001 - Strange Bass JR",
		"B2 P55 Alpha Lead.syx,B055 - Alpha Lead JR",
		"Stranger_Things.syx,B108 - StrangerThings"
	})
	public void should_return_full_patch_name(String file, String name) throws Exception {
		//GIVEN a P'08 file
		URI uri = getFileURI(file);
		ByteBuffer patch = DSISysexLoader.loadSysexFile(uri);
		//WHEN we call the method
		String fullName = DSISysexLoader.getFullName(patch);
		//THEN it should return the full patch name and position
		assertThat(name.equals(fullName)).isTrue();
	}


	@ParameterizedTest
	@CsvSource({
		"Stranger_Things.syx,false",
		"B2_P01_Strange_Bass.syx,false",
		"Prophet_08_Programs_v1.0.syx,true",
		"Rev2_Programs_v1.0.syx,true"
	})
	public void should_identify_multipatch_sysex_file(String file, Boolean status) throws Exception {
		//GIVEN
		URI uri = getFileURI(file);
		ByteBuffer sysex = DSISysexLoader.loadSysexFile(uri);
		//WHEN
		//THEN
		assertThat(DSISysexLoader.isMultipatchFile(sysex)).isEqualTo(status);
	}
	
	
	@ParameterizedTest
	@CsvSource({
		"B2_P01_Strange_Bass.syx,D128,B4_P128_Strange_Bass.syx",
		"B2_P01_Strange_Bass.syx,A042,B1_P42_Strange_Bass.syx"
	})
	public void should_change_position_on_existing_patch(String file, String newPos, String goldenMaster) throws Exception {
		//GIVEN
		URI uri = getFileURI(file);
		ByteBuffer sysex = DSISysexLoader.loadSysexFile(uri);
		URI uri2 = getFileURI(goldenMaster);
		ByteBuffer gold = DSISysexLoader.loadSysexFile(uri2); 
		//WHEN
		ByteBuffer newfile = DSISysexLoader.setNewPosition(sysex,newPos);
		//THEN
		assertThat(newfile).isEqualByComparingTo(gold);		
	}
	
	//FIXME one test for garbage (newpos being too short, too long, or something similar)
	@ParameterizedTest
	@CsvSource({
		"B2_P01_Strange_Bass.syx,E142",
		"B2_P01_Strange_Bass.syx,A24"
	})
	public void should_throw_exception_on_wrong_newpos(String file, String newPos) throws Exception {
		//GIVEN
		URI uri = getFileURI(file);
		ByteBuffer sysex = DSISysexLoader.loadSysexFile(uri);
		//WHEN
		//THEN
		assertThatExceptionOfType(IllegalArgumentException.class).isThrownBy(() -> DSISysexLoader.setNewPosition(sysex,newPos)).withMessage("Illegal new position: "+newPos);
	}
}
