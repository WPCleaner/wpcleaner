/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2011  Nicolas Vervelle
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

package org.wikipediacleaner.api.constants.wiki;


/**
 * Abstract base class for Wikiversity configuration.
 */
public abstract class AbstractWikiversitySettings
  extends AbstractWikiSettings {

  /**
   * @param language Wikiversity language.
   */
  protected AbstractWikiversitySettings(
      String language,
      String name) {
    this.language = language;
    this.code = "v:" + language;
    this.codeCheckWiki = language + "wikiversity";
    this.name = name;
    this.hostUrl = "http://" + language + ".wikiversity.org";
    this.apiUrl = hostUrl + "/w/api.php";
    this.indexUrl = hostUrl + "/w/index.php";
  }

  /**
   * Wikiversity language.
   */
  private final String language;

  /**
   * @return Wikiversity language.
   */
  @Override
  public String getLanguage() {
    return language;
  }

  /**
   * Wikiversity code.
   */
  private final String code;

  /**
   * @return Wikiversity code.
   */
  @Override
  public String getCode() {
    return code;
  }

  /**
   * CheckWiki code.
   */
  private final String codeCheckWiki;

  /**
   * @return CheckWiki code.
   */
  @Override
  public String getCodeCheckWiki() {
    return codeCheckWiki;
  }

  /**
   * Wikiversity name.
   */
  private final String name;

  /**
   * @return Wikiversity name.
   */
  @Override
  public String getName() {
    return name;
  }

  /**
   * Host URL (URL to Wikiversity host).
   */
  private final String hostUrl;

  /**
   * @return Host URL (URL to Wikiversity host).
   */
  @Override
  public String getHostURL() {
    return hostUrl;
  }

  /**
   * API URL (URL to api.php).
   */
  private final String apiUrl;

  /**
   * @return API URL (URL to api.php).
   */
  @Override
  public String getApiURL() {
    return apiUrl;
  }

  /**
   * Index URL (URL to index.php).
   */
  private final String indexUrl;

  /**
   * @return Index URL (URL to index.php).
   */
  @Override
  public String getIndexURL() {
    return indexUrl;
  }
}
