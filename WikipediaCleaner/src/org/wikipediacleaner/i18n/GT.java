/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.i18n;

import java.text.MessageFormat;
import java.util.Locale;

import org.wikipediacleaner.api.constants.EnumLanguage;
import org.wikipediacleaner.utils.Configuration;
import org.xnap.commons.i18n.I18n;
import org.xnap.commons.i18n.I18nFactory;


/**
 * Internationalization management.
 */
public class GT {

  private static GT getTextWrapper;

  private final I18n i18n;

  private EnumLanguage language;

  /**
   * Constructor.
   */
  private GT() {
    Configuration config = Configuration.getConfiguration();
    language = config.getLanguage();
    i18n = I18nFactory.getI18n(getClass(), "org.wikipediacleaner.i18n.Messages", language.getLocale());
  }

  /**
   * @param msg Original text.
   * @return Translated text.
   */
  public static String _T(String msg) {
    return getTextWrapper().getString(msg);
  }

  /**
   * @param msg Original text (with variables place holders).
   * @param variable Variable value.
   * @return Translated text.
   */
  public static String _T(String msg, String variable) {
    return getTextWrapper().getString(msg, new Object[] { variable });
  }

  /**
   * @param msg Original text (with variables place holders).
   * @param msgPlural Original text in plural (with variables place holders).
   * @param n Plural indicator.
   * @param variable Variable value.
   * @return Translated text.
   */
  public static String __(String msg, String msgPlural, long n, String variable) {
    return getTextWrapper().getString(msg, msgPlural, n, new Object[] { variable });
  }

  /**
   * @param msg Original text (with variables place holders).
   * @param objects Variables values.
   * @return Translated text.
   */
  public static String _T(String msg, Object[] objects) {
    return getTextWrapper().getString(msg, objects);
  }

  /**
   * @param msg Original text (with variables place holders).
   * @param msgPlural Original text in plural (with variables place holders).
   * @param n Plural indicator.
   * @param objects Variables values.
   * @return Translated text.
   */
  public static String __(String msg, String msgPlural, long n, Object[] objects) {
    return getTextWrapper().getString(msg, msgPlural, n, objects);
  }

  /**
   * Utility function to mark a string as translatable without translating it.
   * 
   * @param msg Original text.
   * @return Original text.
   */
  public static String _No(String msg) {
    return msg;
  }

  /**
   * @return Wrapper.
   */
  private static GT getTextWrapper() {
    if (getTextWrapper == null) {
      getTextWrapper = new GT();
    }
    return getTextWrapper;
  }

  /**
   * @param msg Original text.
   * @return Translated text.
   */
  private String getString(String msg) {
    return i18n.tr(msg);
  }

  /**
   * @param msg Original text.
   * @param msgPlural Original text in plural.
   * @param n Plural indicator.
   * @return Translated text.
   */
  private String getString(String msg, String msgPlural, long n) {
    return i18n.trn(msg, msgPlural, n);
  }

  /**
   * @param msg Original text (with variables place holders).
   * @param objects Variables values.
   * @return Translated text.
   */
  private String getString(String msg, Object[] objects) {
    return i18n.tr(msg, objects);
  }

  /**
   * @param msg Original text (with variables place holders).
   * @param msgPlural Original text in plural (with variables place holders).
   * @param n Plural indicator.
   * @param objects Variables values.
   * @return Translated text.
   */
  private String getString(String msg, String msgPlural, long n, Object[] objects) {
    if (msg == null) {
      return null;
    }
    String txt = null;
    if ((language == null) || (language == EnumLanguage.EN)) {
      txt = (n == 1) ? msg : msgPlural;
    } else {
      txt = getString(msg, msgPlural, n);
    }
    return MessageFormat.format(txt, objects);
  }

  /**
   * Change the current language.
   * 
   * @param language Current language.
   */
  private void setLanguage(EnumLanguage language) {
    Locale locale = (language != null) ? language.getLocale() : Locale.getDefault();
    Locale.setDefault(locale);
    this.language = language;
    i18n.setLocale(locale);
  }

  /**
   * Change the current language.
   * 
   * @param language Current language.
   */
  public static void setCurrentLanguage(EnumLanguage language) {
    getTextWrapper().setLanguage(language);
  }
}
