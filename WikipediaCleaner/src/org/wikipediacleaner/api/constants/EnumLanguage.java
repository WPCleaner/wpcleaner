/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2007  Nicolas Vervelle
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

package org.wikipediacleaner.api.constants;

import java.util.Locale;
import java.util.ResourceBundle;
import java.util.Vector;

import org.wikipediacleaner.i18n.Messages;


/**
 * Encapsulate possible languages.
 */
public enum EnumLanguage {

  CS   ("cs"   , "Czech"),
  DA   ("da"   , "Danish"),
  DE   ("de"   , "German"),
  EN   ("en"   , "English"),
  ES   ("es"   , "Spanish"),
  FR   ("fr"   , "Français"),
  HE   ("he"   , "Hebrew"),
  HU   ("hu"   , "Hungarian"),
  ID   ("id"   , "Indonesian"),
  IS   ("is"   , "Icelandic"),
  IT   ("it"   , "Italian"),
  KO   ("ko"   , "Korean"),
  NB   ("nb"   , "Norwegian Bokmal"),
  NL   ("nl"   , "Dutch"),
  OC   ("oc"   , "Occitan"),
  PL   ("pl"   , "Polish"),
  PT_BR("pt_BR", "Brazilian Portuguese"),
  RU   ("ru"   , "Russian"),
  SV   ("sv"   , "Swedish"),
  TR   ("tr"   , "Turkish");

  private final String code;
  private final String language;
  private final ResourceBundle bundle;

  /**
   * @param code Code.
   * @param language Language.
   */
  EnumLanguage(String code, String language) {
    this.code = code;
    this.language = language;
    ResourceBundle tmpBundle = null;
    try {
      Class bundleClass = Class.forName(Messages.class.getName() + "_" + code);
      if ((bundleClass != null) &&
          (ResourceBundle.class.isAssignableFrom(bundleClass))) {
        tmpBundle = (ResourceBundle) bundleClass.newInstance();
      }
    } catch (ClassNotFoundException e) {
      // No bundle, we simply don't have translation for this language
    } catch (IllegalAccessException e) {
      // Shouldn't happen, but same consequence as ClassNotFoundException
    } catch (InstantiationException e) {
      // Shouldn't happen, but same consequence as ClassNotFoundException
    }
    bundle = tmpBundle;
  }

  /**
   * @return Vector of all languages.
   */
  public static Vector<EnumLanguage> getVector() {
    Vector<EnumLanguage> vector = new Vector<EnumLanguage>(EnumLanguage.values().length);
    for (EnumLanguage e : EnumLanguage.values()) {
      vector.add(e);
    }
    return vector;
  }

  /**
   * @param code Requested code language.
   * @return Corresponding language.
   */
  public static EnumLanguage getLanguage(String code) {
    for (EnumLanguage e : EnumLanguage.values()) {
      if (e.getCode().equals(code)) {
        return e;
      }
    }
    return getDefaultLanguage();
  }

  /**
   * @return Language.
   */
  public static EnumLanguage getDefaultLanguage() {
    Locale locale = Locale.getDefault();
    if ((locale != null) && (locale.getLanguage() != null)) {
      for (EnumLanguage e : EnumLanguage.values()) {
        if (e.getCode().equals(locale.getLanguage())) {
          return e;
        }
      }
    }
    return EN;
  }

  /**
   * @return Code.
   */
  public String getCode() {
    return code;
  }

  /**
   * @return Language.
   */
  public String getLanguage() {
    return language;
  }

  /**
   * @return Resource bundle for translation.
   */
  public final ResourceBundle getResourceBundle() {
    return bundle;
  }

  /* (non-Javadoc)
   * @see java.lang.Enum#toString()
   */
  @Override
  public String toString() {
    return getCode() + " - " + getLanguage();
  }
}
