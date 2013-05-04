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

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.ResourceBundle;

import org.wikipediacleaner.i18n.Messages;


/**
 * Encapsulate possible languages.
 */
public enum EnumLanguage {

  CS   (new Locale("cs")      , "Czech"),
  DA   (new Locale("da")      , "Danish"),
  DE   (Locale.GERMAN         , "German"),
  EL   (new Locale("el")      , "Greek"),
  EN   (Locale.ENGLISH        , "English"),
  EO   (new Locale("eo")      , "Esperanto"),
  ES   (new Locale("es")      , "Spanish"),
  FR   (Locale.FRENCH         , "Français"),
  HE   (new Locale("he")      , "Hebrew"),
  HU   (new Locale("hu")      , "Hungarian"),
  ID   (new Locale("id")      , "Indonesian"),
  IS   (new Locale("is")      , "Icelandic"),
  IT   (Locale.ITALIAN        , "Italian"),
  JA   (Locale.JAPANESE       , "Japanese"),
  KO   (Locale.KOREAN         , "Korean"),
  NB   (new Locale("nb")      , "Norwegian Bokmal"),
  NL   (new Locale("nl")      , "Dutch"),
  OC   (new Locale("oc")      , "Occitan"),
  PL   (new Locale("pl")      , "Polish"),
  PT_BR(new Locale("pt", "BR"), "Brazilian Portuguese"),
  RU   (new Locale("ru")      , "Russian"),
  SV   (new Locale("sv")      , "Swedish"),
  TR   (new Locale("tr")      , "Turkish"),
  UK   (new Locale("uk")      , "Ukrainian");

  private final String code;
  private final Locale locale;
  private final String language;
  private final ResourceBundle bundle;

  /**
   * @param locale Locale.
   * @param language Language.
   */
  EnumLanguage(Locale locale, String language) {
    this.code = locale.toString();
    this.locale = locale;
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
  public static List<EnumLanguage> getList() {
    List<EnumLanguage> list = new ArrayList<EnumLanguage>(EnumLanguage.values().length);
    for (EnumLanguage e : EnumLanguage.values()) {
      list.add(e);
    }
    return list;
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
   * @return Locale.
   */
  public Locale getLocale() {
    return locale;
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
