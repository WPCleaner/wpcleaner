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
import java.util.Map;

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
  private final static Map<EnumWikipedia, String> localAuthorizedCharacters =
    new HashMap<EnumWikipedia, String>();

  /**
   * Possible replacements for unauthorized characters.
   */
  private final static Map<Character, String> replacements = new HashMap<Character, String>();

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
    replacements.put((char) 0xFEFF, "");
    replacements.put((char) 0x200E, "");
    replacements.put((char) 0x200B, "");
    addReplacements("’", "'");

    addReplacements("ÀÁÂÃĀĂȦÄẢÅǍẠĄ", "A");
    addReplacements("àáâãāăȧäảåǎạą", "a");
    addReplacements("ÆǼǢ", "AE");
    addReplacements("æǽǣ", "ae");

    addReplacements("ḂƁḄ", "B");
    addReplacements("ḃɓḅ", "b");

    addReplacements("ĆḈĈĊƇČÇ", "C");
    addReplacements("ćḉĉċƈčç", "c");

    addReplacements("ḊƊĎḌḐ", "D");
    addReplacements("ḋɗďḍḑ", "d");

    addReplacements("ÈÉÊẼỄĒĔĖËẺỂĚẸỆȨĘę", "E");
    addReplacements("èéêẽễēĕėëẻểěẹệȩ", "e");

    addReplacements("ḞƑ", "F");
    addReplacements("ḟƒ", "f");

    addReplacements("ǴĜḠĞĠƓǦĢ", "G");
    addReplacements("ǵĝḡğġɠǧģ", "g");

    addReplacements("ĤḢḦȞḤḨ", "H");
    addReplacements("ĥḣḧɦȟḥḩ", "h");

    addReplacements("ÌÍÎĨĪĬİÏǏỊĮ", "I");
    addReplacements("ìíîĩīĭıïǐịį", "i");

    addReplacements("Ĵ", "J");
    addReplacements("ĵǰ", "j");

    addReplacements("ḰƘǨḲĶ", "K");
    addReplacements("ḱƙǩḳķ", "k");

    addReplacements("ĹĿĽḶĻ", "L");
    addReplacements("ĺŀľḷļł", "l");

    addReplacements("ḾṀṂ", "M");
    addReplacements("ḿṁɱṃ", "m");

    addReplacements("ǸŃÑṄŇṆŅ", "N");
    addReplacements("ǹńñṅňṇņ", "n");

    addReplacements("ÒÓÔÕŌŎȮÖỎŐǑỌǪ", "O");
    addReplacements("òóôõōŏȯöỏőǒọǫ", "o");
    addReplacements("Œ", "OE");
    addReplacements("œ", "oe");

    addReplacements("ṔṖƤ", "P");
    addReplacements("ṕṗ", "p");

    addReplacements("ŔṘŘṚŖ", "R");
    addReplacements("ŕṙřṛŗ", "r");

    addReplacements("ŚŜṠŠṢȘŞ", "S");
    addReplacements("śŝṡšṣșş", "s");

    addReplacements("ṪƬŤṬŢŢ", "T");
    addReplacements("ṫẗƭťṭţţ", "t");

    addReplacements("ÙÚÛŨŪŬÜŮŰǓỤŲ", "U");
    addReplacements("ùúûũūŭüůűǔụų", "u");

    addReplacements("ṼṾ", "V");
    addReplacements("ṽṿ", "v");

    addReplacements("ẀẂŴẆẄẈ", "W");
    addReplacements("ẁẃŵẇẅẘẉ", "w");

    addReplacements("ẊẌ", "X");
    addReplacements("ẋẍ", "x");

    addReplacements("ỲÝŶỸȲẎŸỴ", "Y");
    addReplacements("ỳýŷỹȳẏÿẙỵ", "y");

    addReplacements("ŹẐŻŽẒ", "Z");
    addReplacements("źẑżẓž", "z");
  }

  /**
   * Add replacements for some characters.
   * 
   * @param characters All the characters to replace.
   * @param replacement Replacement.
   */
  private static void addReplacements(String characters, String replacement) {
    for (int i = 0; i < characters.length(); i++) {
      replacements.put(characters.charAt(i), replacement);
    }
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
