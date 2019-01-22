/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
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

  AST  (new Locale("ast")     , "Asturian"),
  AZ   (new Locale("az")      , "Azerbaijani"),
  BN   (new Locale("bn")      , "Bengali"),
  CS   (new Locale("cs")      , "Czech"),
  DA   (new Locale("da")      , "Danish"),
  DE   (Locale.GERMAN         , "German"),
  EL   (new Locale("el")      , "Greek"),
  EN   (Locale.ENGLISH        , "English"),
  EN_GB(new Locale("en_GB")   , "English (United Kingdom)"),
  EO   (new Locale("eo")      , "Esperanto"),
  ES   (new Locale("es")      , "Spanish"),
  FA   (new Locale("fa")      , "Persian"),
  FR   (Locale.FRENCH         , "Français"),
  GL   (new Locale("gl")      , "Galician"),
  HE   (new Locale("he")      , "Hebrew"),
  HU   (new Locale("hu")      , "Hungarian"),
  ID   (new Locale("id")      , "Indonesian"),
  IS   (new Locale("is")      , "Icelandic"),
  IT   (Locale.ITALIAN        , "Italian"),
  JA   (Locale.JAPANESE       , "Japanese"),
  KO   (Locale.KOREAN         , "Korean"),
  MR   (new Locale("mr")      , "Marathi"),
  MS   (new Locale("ms")      , "Malay"),
  NB   (new Locale("nb")      , "Norwegian Bokmal"),
  NL   (new Locale("nl")      , "Dutch"),
  OC   (new Locale("oc")      , "Occitan"),
  PL   (new Locale("pl")      , "Polish"),
  PS   (new Locale("ps")      , "Pashto"),
  PT_BR(new Locale("pt", "BR"), "Brazilian Portuguese"),
  RU   (new Locale("ru")      , "Russian"),
  SV   (new Locale("sv")      , "Swedish"),
  TR   (new Locale("tr")      , "Turkish"),
  UK   (new Locale("uk")      , "Ukrainian"),
  UR   (new Locale("ur")      , "Urdu"),
  ZH_CN(new Locale("zh", "CN"), "Chinese (Simplified)");

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
    } catch (UnsupportedClassVersionError e) {
      // May happen if messages compiled with a more recent Java, but same consequence as ClassNotFoundException
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
