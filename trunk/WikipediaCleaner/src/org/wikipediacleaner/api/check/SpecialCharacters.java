/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2008  Nicolas Vervelle
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.wikipediacleaner.api.check;

import java.util.HashMap;

import org.wikipediacleaner.api.constants.EnumWikipedia;


/**
 * Management of special characters.
 */
public class SpecialCharacters {

  /**
   * Punctuation characters. 
   */
  private final static String punctuation = ".,;:!?";

  /**
   * Characters authorized for every Wikipedia.
   */
  private final static String authorizedCharacters =
    "abcdefghijklmnopqrstuvwxyz" +
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ" +
    " 0123456789-:,.!?'&#/()*";

  /**
   * Characters authorized for specific Wikipedia. 
   */
  private final static HashMap<EnumWikipedia, String> localAuthorizedCharacters =
    new HashMap<EnumWikipedia, String>();

  /**
   * Possible replacements for unauthorized characters.
   */
  private final static HashMap<Character, String> replacements = new HashMap<Character, String>();

  static {
    // Locally authorized characters
    localAuthorizedCharacters.put(EnumWikipedia.CS, "čďěňřšťžČĎŇŘŠŤŽ");
    localAuthorizedCharacters.put(EnumWikipedia.DA, "ÆØÅæøå");
    localAuthorizedCharacters.put(EnumWikipedia.FI, "ÅÄÖåäö");
    //localAuthorizedCharacters.put(EnumWikipedia.NN, "ÆØÅæøå");
    localAuthorizedCharacters.put(EnumWikipedia.NO, "ÆØÅæøå");
    localAuthorizedCharacters.put(EnumWikipedia.RO, "ăîâşţ");
    localAuthorizedCharacters.put(EnumWikipedia.RU, "АБВГДЕЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯабвгдежзийклмнопрстуфхцчшщьыъэюя");
    localAuthorizedCharacters.put(EnumWikipedia.SV, "ÅÄÖåäö");
    localAuthorizedCharacters.put(EnumWikipedia.UK, "АБВГДЕЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯабвгдежзийклмнопрстуфхцчшщьыъэюяіїґ");

    // Possible replacements
    replacements.put('Á', "A");
    replacements.put('À', "A");
    replacements.put('Â', "A");
    replacements.put('Ä', "A");
    replacements.put('á', "a");
    replacements.put('à', "a");
    replacements.put('â', "a");
    replacements.put('ä', "a");
    replacements.put('Æ', "AE");
    replacements.put('æ', "ae");

    replacements.put('Ç', "C");
    replacements.put('ç', "c");

    replacements.put('É', "E");
    replacements.put('È', "E");
    replacements.put('Ê', "E");
    replacements.put('Ë', "E");
    replacements.put('é', "e");
    replacements.put('è', "e");
    replacements.put('ê', "e");
    replacements.put('ë', "e");

    replacements.put('Î', "I");
    replacements.put('Ï', "I");
    replacements.put('í', "i");
    replacements.put('î', "i");
    replacements.put('ï', "i");

    replacements.put('ñ', "n");

    replacements.put('Ó', "O");
    replacements.put('Ô', "O");
    replacements.put('Ö', "O");
    replacements.put('ó', "o");
    replacements.put('ô', "o");
    replacements.put('ö', "o");
    replacements.put('Œ', "OE");
    replacements.put('œ', "oe");

    replacements.put('Ù', "U");
    replacements.put('Û', "U");
    replacements.put('Ü', "U");
    replacements.put('ú', "u");
    replacements.put('ù', "u");
    replacements.put('û', "u");
    replacements.put('ü', "u");

    replacements.put('Ÿ', "Y");
    replacements.put('ÿ', "y");
  }

  /**
   * @param character Character tested.
   * @return Flag indicating if the character is a punctuation.
   */
  public static boolean isPunctuation(char character) {
    if (punctuation.indexOf(character) >= 0) {
      return true;
    }
    return false;
  }

  /**
   * @param character Character tested.
   * @param code Wikipedia code.
   * @return Flag indicating if the character is authorized.
   */
  public static boolean isAuthorized(char character, EnumWikipedia code) {
    if (authorizedCharacters.indexOf(character) >= 0) {
      return true;
    }
    if (code == null) {
      return false;
    }
    String local = localAuthorizedCharacters.get(code);
    if (local == null) {
      return false;
    }
    return (local.indexOf(character) >= 0);
  }

  /**
   * @param character Character to be replaced.
   * @return Replacement.
   */
  public static String proposeReplacement(char character) {
    String replacement = replacements.get(Character.valueOf(character));
    if (replacement != null) {
      return replacement;
    }
    return Character.toString(character);
  }
}
