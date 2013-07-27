/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
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

package org.wikipediacleaner.i18n;

import java.text.MessageFormat;
import java.util.Locale;

import org.wikipediacleaner.api.constants.EnumLanguage;
import org.xnap.commons.i18n.I18n;
import org.xnap.commons.i18n.I18nFactory;


/**
 * Internationalization management.
 */
public class GT {

  private static GT getTextWrapper;

  private final I18n i18n;

  /**
   * Constructor.
   */
  private GT() {
    i18n = I18nFactory.getI18n(getClass(), "org.wikipediacleaner.i18n.Messages", Locale.getDefault());
  }

  /**
   * @param msg Original text.
   * @return Translated text.
   */
  public static String _(String msg) {
    return getTextWrapper().getString(msg);
  }

  /**
   * @param msg Original text (with variables place holders).
   * @param variable Variable value.
   * @return Translated text.
   */
  public static String _(String msg, String variable) {
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
  public static String _(String msg, Object[] objects) {
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
    String txt = getString(msg, msgPlural, n);
    return MessageFormat.format(txt, objects);
  }

  /**
   * Change the current language.
   * 
   * @param language Current language.
   */
  public static void setCurrentLanguage(EnumLanguage language) {
    Locale locale = (language != null) ? language.getLocale() : Locale.getDefault();
    getTextWrapper().i18n.setLocale(locale);
  }
}
