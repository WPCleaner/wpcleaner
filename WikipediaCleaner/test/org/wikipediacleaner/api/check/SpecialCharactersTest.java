/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check;

import static org.junit.Assert.*;

import org.junit.Test;
import org.wikipediacleaner.api.constants.EnumWikipedia;


/**
 * Test class for SpecialCharacters.
 */
public class SpecialCharactersTest {
  
  @Test
  public void testProposeReplacement() {
    assertEquals("A", SpecialCharacters.proposeReplacement('A', EnumWikipedia.FR));
    assertEquals("ά", SpecialCharacters.proposeReplacement('ά', EnumWikipedia.FR));
    assertEquals("α", SpecialCharacters.proposeReplacement('ά', EnumWikipedia.EL));
  }

  @Test
  public void testReplaceAllSpecialCharacters() {
    assertEquals("Ilot Saint-Eloi", SpecialCharacters.replaceAllSpecialCharacters("Îlot Saint-Éloi", EnumWikipedia.FR));
  }
}
