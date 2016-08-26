/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data;

import java.util.List;


/**
 * Information about languages.
 */
public class Language implements Comparable<Language> {

  private final String code;
  private final String name;

  /**
   * @param languages List of languages.
   * @param code Language code;
   * @return Language exist ?
   */
  public static boolean isLanguageCode(List<Language> languages, String code) {
    if ((languages == null) || (code == null)) {
      return false;
    }
    for (Language language : languages) {
      if ((language != null) &&
          areCodesEqual(code, language.getCode())) {
        return true;
      }
    }
    return false;
  }

  /**
   * @param code Language code.
   * @param name Language name.
   */
  public Language(String code, String name) {
    this.code = code;
    this.name = name;
  }

  /**
   * @return Language code.
   */
  public String getCode() {
    return code;
  }

  /**
   * @return Language name.
   */
  public String getName() {
    return name;
  }

  /**
   * Compare language codes.
   * 
   * @param code1 First language code.
   * @param code2 Second language code.
   * @return True if language codes are equal.
   */
  public static boolean areCodesEqual(String code1, String code2) {
    if (code1 == null) {
      return (code2 == null);
    }
    if (code2 == null) {
      return false;
    }
    if (code1.equals(code2)) {
      return true;
    }
    if ((code1.length() == 0) || (code2.length() == 0)) {
      return false;
    }
    if (code1.length() != code2.length()) {
      return false;
    }
    code1 = Character.toUpperCase(code1.charAt(0)) + code1.substring(1);
    code2 = Character.toUpperCase(code2.charAt(0)) + code2.substring(1);
    return code1.equals(code2);
  }

  /* (non-Javadoc)
   * @see java.lang.Comparable#compareTo(java.lang.Object)
   */
  @Override
  public int compareTo(Language lg) {
    int compare;

    // Code
    compare = code.compareTo(lg.code);
    if (compare != 0) {
      return compare;
    }

    // Name
    compare = name.compareTo(lg.name);
    
    return compare;
  }

  /* (non-Javadoc)
   * @see java.lang.Object#equals(java.lang.Object)
   */
  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if ((o == null) || (o.getClass() != getClass())) {
      return false;
    }
    Language lg = (Language) o;
    boolean equals = true;
    equals &= code.equals(lg.code);
    equals &= name.equals(lg.name);
    return equals;
  }

  /* (non-Javadoc)
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode() {
    int hash = 7;
    hash = 31 * hash + code.hashCode();
    hash = 31 * hash + name.hashCode();
    return hash;
  }
}
