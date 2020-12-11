/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpStatus;
import org.apache.commons.httpclient.methods.GetMethod;
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
   * Characters authorized for every Wiki.
   */
  private final static String authorizedCharacters =
    "abcdefghijklmnopqrstuvwxyz" +
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ" +
    " 0123456789-:,.!?'#/()*";

  /**
   * Characters authorized for specific Wiki. 
   */
  private final static Map<EnumWikipedia, String> localAuthorizedCharacters =
    new HashMap<>();

  /**
   * Possible replacements for unauthorized characters for every Wiki.
   */
  private final static Map<Integer, String> replacements = new HashMap<>();

  /**
   * Possible replacements for unauthorized characters for specific Wiki.
   */
  private final static Map<EnumWikipedia, Map<Integer, String>> localReplacements =
      new HashMap<>();

  /**
   * Set to true to check all replacements.
   */
  private final static boolean CHECK_REPLACEMENTS = false;

  static {

    // Possible replacements for every Wiki
    replacements.put(0xFEFF, "");
    replacements.put(0x200E, "");
    replacements.put(0x200B, "");
    addReplacements("ʻ’“”„‟′", "'");

    addReplacements("ÀÁÂÃÄÅĀĂĄǍǞǠǺȀȂȦȺАḀẠẢẤẦẨẪẬẮẰẲẴẶ", "A");
    addReplacements("àáâãäåāăąǎǟǡǻȁȃȧаḁẚạảấầẩẫậắằẳẵặⱥ", "a");
    addReplacements("ÆǢǼ", "Ae");
    addReplacements("æǣǽ", "ae");

    addReplacements("ƁɃḂḄḆ", "B");
    addReplacements("ƀɓбᶀḃḅḇ", "b");

    addReplacements("ÇĆĈĊČƇȻḈ", "C");
    addReplacements("çćĉċčƈȼḉ", "c");

    addReplacements("ÐĎĐƉƊƋḊḌḎḐ", "D");
    addReplacements("ðďđƌȡɖɗᶁḋḍḏḑ", "d");
    addReplacements("ǅǲ", "Dz");

    addReplacements("ÈÉÊËĒĔĖĘĚƎƏƐȄȆȨɆΕЁЕḘḚẸẺẼẾỀỂỄỆ", "E");
    addReplacements("èéêëēĕėęěǝȅȇȩɇəеёḕḗḙḛḝẹẻẽếềểễệ", "e");

    addReplacements("ƑḞ", "F");
    addReplacements("ƒᶂḟ", "f");

    addReplacements("ĜĞĠĢƓǤǦǴḠ", "G");
    addReplacements("ĝğġģǥǧǵɠᶃḡ", "g");

    addReplacements("ĤĦȞḢḤḦḨḪⱧΗ", "H");
    addReplacements("ĥħȟɦḣḥḧḩḫẖⱨ", "h");

    addReplacements("ÌÍÎÏĨĪĬĮİƗǏȈȊІỊ", "I");
    addReplacements("ìíîïĩīĭįıǐȉȋɨіỉị", "i");

    addReplacements("ĴɈ", "J");
    addReplacements("ĵǰɉ", "j");

    addReplacements("ĶƘǨКḰḲḴⱩ", "K");
    addReplacements("ķƙǩкᶄḱḳḵⱪ", "k");

    addReplacements("ĹĻĽĿŁȽḶḸḺⱠⱢ", "L");
    addReplacements("ĺļľŀłƚȴлᶅḷḹḻⱡ", "l");
    addReplacements("ǈ", "Lj");
    addReplacements("ǉ", "lj");

    addReplacements("МḾṀṂ", "M");
    addReplacements("ɱмᶆḿṁṃ", "m");

    addReplacements("ÑŃŅŇƝǸȠṄṆṈ", "N");
    addReplacements("ñńņňŊŋƞǹȵɲᶇṅṇṉ", "n");
    addReplacements("ǋ", "Nj");
    addReplacements("ǌ", "nj");

    addReplacements("ÒÓÔÕÖØŌŎŐƟƠǑǪǬǾȌȎȪȬȮȰОỌỎỐỒỔỖỘỚỜỞỠỢ", "O");
    addReplacements("òóôõöøōŏőơǒǫǭǿȍȏȫȭȯȱɵоọỏốồổỗộớờởỡợ", "o");
    addReplacements("Œ", "Oe");
    addReplacements("œ", "oe");

    addReplacements("ƤṔṖⱣ", "P");
    addReplacements("ƥᵽᶈṕṗ", "p");

    addReplacements("ʠ", "q");

    addReplacements("ŔŖŘȐȒɌṘṚṜṞⱤ", "R");
    addReplacements("ŕŗřȑȓɍᶉṙṛṝṟ", "r");

    addReplacements("ŚŜŞŠȘṠṢ", "S");
    addReplacements("śŝşšșȿᶊṡṣ", "s");
    addReplacements("ß", "ss");

    addReplacements("ŢŤŦƬƮȚȾṪṬṮṰ", "T");
    addReplacements("ţťŧƫƭțȶʈṫṭṯṱẗⱦ", "t");
    addReplacements("Þ", "Th");
    addReplacements("þ", "th");

    addReplacements("ÙÚÛÜŨŪŬŮŰŲƯǓǕǗǙǛȔȖɄṲṶỤỨỪỬỮỰ", "U");
    addReplacements("ùúûüũūŭůűųưǔǖǘǚǜȕȗʉṳṷụủứừửữự", "u");

    addReplacements("ƲṼṾ", "V");
    addReplacements("ʋṽṿⱴ", "v");

    addReplacements("ŴƜẀẂẄẆẈⱲ", "W");
    addReplacements("ŵẁẃẅẇẉẘⱳ", "w");

    addReplacements("ХẊẌ", "X");
    addReplacements("хᶍẋẍ", "x");

    addReplacements("ÝŶŸƳȲɎӲẎỲỴỶỸ", "Y");
    addReplacements("ýÿŷƴȳɏӳᶌẏẙỳỵỷỹ", "y");

    addReplacements("ŹŻŽƵȤẐẒẔⱫ", "Z");
    addReplacements("źżžƶȥɀʐᶎẑẓⱬ", "z");

    addReplacements("²", "2");
    addReplacements("–—−", "-");
    addReplacements("…", "...");

    // Specific configuration for CS
    localAuthorizedCharacters.put(EnumWikipedia.CS, "čďěňřšťžČĎŇŘŠŤŽ");

    // Specific configuration for DA
    localAuthorizedCharacters.put(EnumWikipedia.DA, "ÆØÅæøå");

    // Specific configuration for EL
    localAuthorizedCharacters.put(EnumWikipedia.EL, "ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩαβγδεζηθικλμνξοπρσςτυφχψω");
    addReplacements(EnumWikipedia.EL, "ά", "α");
    addReplacements(EnumWikipedia.EL, "έ", "ε");
    addReplacements(EnumWikipedia.EL, "ή", "η");
    addReplacements(EnumWikipedia.EL, "ί", "ι");
    addReplacements(EnumWikipedia.EL, "ϊ", "ι");
    addReplacements(EnumWikipedia.EL, "ό", "ο");
    addReplacements(EnumWikipedia.EL, "ύ", "υ");
    addReplacements(EnumWikipedia.EL, "ϋ", "υ");
    addReplacements(EnumWikipedia.EL, "ώ", "ω");
    addReplacements(EnumWikipedia.EL, "Ά", "Α");
    addReplacements(EnumWikipedia.EL, "Έ", "Ε");
    addReplacements(EnumWikipedia.EL, "Ή", "Η");
    addReplacements(EnumWikipedia.EL, "Ί", "Ι");
    addReplacements(EnumWikipedia.EL, "Ό", "Ο");
    addReplacements(EnumWikipedia.EL, "Ύ", "Υ");
    addReplacements(EnumWikipedia.EL, "Ώ", "Ω");

    // Specific configuration for EN
    // TODO: Correctly handle HTML entity...
    // addReplacements(EnumWikipedia.EN, "&", "And");

    // Specific configuration for ES
    addReplacements(EnumWikipedia.ES, "Ñ", "Nzz");
    addReplacements(EnumWikipedia.ES, "ñ", "nzz");

    // Specific configuration for FI
    localAuthorizedCharacters.put(EnumWikipedia.FI, "ÅÄÖåäö");

    // Specific configuration for NN
    //localAuthorizedCharacters.put(EnumWikipedia.NN, "ÆØÅæøå");

    // Specific configuration for NO
    localAuthorizedCharacters.put(EnumWikipedia.NO, "ÆØÅæøå");

    // Specific configuration for RO
    localAuthorizedCharacters.put(EnumWikipedia.RO, "ăîâşţ");

    // Specific configuration for RU
    localAuthorizedCharacters.put(EnumWikipedia.RU, "АБВГДЕЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯабвгдежзийклмнопрстуфхцчшщьыъэюя");

    // Specific configuration for SV
    localAuthorizedCharacters.put(EnumWikipedia.SV, "ÅÄÖåäö");

    // Specific configuration for UK
    localAuthorizedCharacters.put(EnumWikipedia.UK, "АБВГДЕЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯабвгдежзийклмнопрстуфхцчшщьыъэюяіїґ");

    if (CHECK_REPLACEMENTS) {
      // Compare with AWB
      BufferedReader br = null;
      GetMethod method = null;
      try {
        HttpClient httpClient = new HttpClient();
        method = new GetMethod("http://sourceforge.net/p/autowikibrowser/code/HEAD/tree/AWB/WikiFunctions/Tools.cs?format=raw");
        int statusCode = httpClient.executeMethod(method);
        if (statusCode == HttpStatus.SC_OK) {
          br = new BufferedReader(new InputStreamReader(method.getResponseBodyAsStream(), "UTF8"));
          String line = null;
          boolean startDiacritics = false;
          boolean endDiacritics = false;
          while (!endDiacritics && ((line = br.readLine()) != null)) {
            if (!startDiacritics) {
              if (line.contains("Diacritics =")) {
                startDiacritics = true;
              }
            } else {
              if (line.contains("}")) {
                endDiacritics = true;
              } else if (line.contains("new KeyValuePair<string, string>")) {
                int quote1 = line.indexOf('\"');
                int quote2 = line.indexOf('\"', quote1 + 1);
                int quote3 = line.indexOf('\"', quote2 + 1);
                int quote4 = line.indexOf('\"', quote3 + 1);
                if ((quote1 >= 0) && (quote2 >= 0) && (quote3 >= 0) && (quote4 >= 0) &&
                    (quote2 == quote1 + 2)) {
                  Integer awbDiacritic = Integer.valueOf(line.charAt(quote1 + 1));
                  String awbReplacement = line.substring(quote3 + 1, quote4);
                  String replacement = replacements.get(awbDiacritic);
                  if (replacement == null) {
                    System.err.println("AWB replacement for " + awbDiacritic + "/" + awbReplacement + " not defined");
                  } else if (!replacement.equals(awbReplacement)) {
                    System.err.println("AWB replacement for " + awbDiacritic + "/" + awbReplacement + " different than " + replacement);
                  }
                }
              }
            }
          }
        } else {
          System.err.println("Error when accessing AWB source code: " + statusCode + " - " + HttpStatus.getStatusText(statusCode));
        }
      } catch (Exception e) {
        System.err.println("Exception when accessing AWB source code (" + e.getClass().getName() + "): " + e.getMessage());
        if (br != null) {
          try {
            br.close();
          } catch (IOException e1) {
            // Do nothing
          }
        }
        if (method != null) {
          method.releaseConnection();
        }
      }
    }
  }

  /**
   * Add replacements for some characters.
   * 
   * @param characters All the characters to replace.
   * @param replacement Replacement.
   */
  private static void addReplacements(String characters, String replacement) {

    // Check a few things : characters in double, characters order, several replacements, ...
    if (CHECK_REPLACEMENTS) {
      for (int i = 0; i < characters.length(); i++) {
        char currentChar = characters.charAt(i);
  
        for (int j = i + 1; j < characters.length(); j++) {
          if (characters.charAt(j) == currentChar) {
            System.err.println(
                "Character in double for " + replacement + ": " +
                currentChar + "/" + i + " = " +
                characters.charAt(j) + "/" + j);
          }
          if (characters.charAt(j) < currentChar) {
            System.err.println(
                "Character " + characters.charAt(j) + "/" + j +
                " for " + replacement +
                " should be before " + currentChar + "/" + i);
          }
        }

        Integer currentInt = Integer.valueOf(currentChar);
        if (replacements.containsKey(currentInt) &&
            !replacement.equals(replacements.get(currentInt))) {
          System.err.println("Several replacements defined for " + currentChar + ":" + replacements.get(currentInt) + "," + replacement);
        }
      }
    }

    // Add the replacement
    characters.chars().forEachOrdered(c -> replacements.put(c, replacement));
  }

  /**
   * Add replacements for some characters for a specific Wiki.
   * 
   * @param wiki Wiki.
   * @param characters All the characters to replace.
   * @param replacement Replacement.
   */
  private static void addReplacements(EnumWikipedia wiki, String characters, String replacement) {
    final Map<Integer, String> localReplacement = localReplacements.computeIfAbsent(wiki, w -> new HashMap<>());
    characters.chars().forEachOrdered(c -> localReplacement.put(c,  replacement));
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
   * @param wiki Wiki.
   * @return Flag indicating if the character is authorized.
   */
  public static boolean isAuthorized(char character, EnumWikipedia wiki) {
    if (authorizedCharacters.indexOf(character) >= 0) {
      return true;
    }
    if (wiki == null) {
      return false;
    }
    String local = localAuthorizedCharacters.get(wiki);
    if (local == null) {
      return false;
    }
    return (local.indexOf(character) >= 0);
  }

  /**
   * @param character Character to be replaced.
   * @param wiki Wiki.
   * @return Replacement if there's one defined.
   */
  private static Optional<String> getReplacement(int character, EnumWikipedia wiki) {
    Map<Integer, String> localReplacement = localReplacements.get(wiki);
    if (localReplacement != null) {
      String replacement = localReplacement.get(Integer.valueOf(character));
      if (replacement != null) {
        return Optional.of(replacement);
      }
    }
    return Optional.ofNullable(replacements.get(Integer.valueOf(character)));
  }

/**
   * @param character Character to be replaced.
   * @param wiki Wiki.
   * @return Replacement.
   */
  public static String proposeReplacement(char character, EnumWikipedia wiki) {
    return getReplacement(character, wiki).orElse(Character.toString(character));
  }

  /**
   * Replace all special characters in a string.
   * 
   * @param initial Initial string.
   * @param wiki Wiki.
   * @return Replacement string.
   */
  public static String replaceAllSpecialCharacters(String initial, EnumWikipedia wiki) {
    StringBuilder result = new StringBuilder();
    initial.chars().forEachOrdered(c ->
      {
        Optional<String> s = getReplacement(c, wiki);
        if (s.isPresent()) {
          result.append(s.get());
        } else {
          result.append((char) c);
        }
      });
    return result.toString();
  }
}
