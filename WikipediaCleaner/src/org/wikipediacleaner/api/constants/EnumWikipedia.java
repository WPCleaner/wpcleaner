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

import java.awt.ComponentOrientation;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Vector;

import org.wikipediacleaner.Version;
import org.wikipediacleaner.api.base.API;
import org.wikipediacleaner.api.base.APIException;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.TemplateMatch;
import org.wikipediacleaner.utils.Configuration;


/**
 * Encapsulate possible Wikipedia sites.
 */
public enum EnumWikipedia {

  /*
   * List of Wikipedia this tool is able to deal with.
   * 
   * For each Wikipedia, the following informations are needed :
   * - 2 letters code for the Wikipedia language.
   * - full Wikipedia name.
   * - full URL of MediaWiki API (api.php).
   * - full URL of the wiki (index.php).
   * - full URL of the help page.
   * - Internal link to the help page.
   * - Component orientation.
   * - "Login successful" text.
   * - "Disambiguation correction" text.
   * - Wiktionary interwiki.
   * - Wiktionary templates.
   * - Templates for indicating that a link to a disambiguation page is normal.
   * - Templates for requiring help on a disambiguation (used for marking links).
   * - Templates for requiring help on a disambiguation (used for getting list of pages requiring help).
   * - Disambiguation project page giving the list of pages to work on.
   * - Array of disambiguation templates.
   * - Array of special templates.
   */

  CA( WikiCa.code, WikiCa.name,
      WikiCa.apiUrl, WikiCa.indexUrl,
      WikiCa.helpUrl, WikiCa.helpLink, WikiCa.orientation,
      WikiCa.logged, WikiCa.message,
      WikiCa.wikt, WikiCa.wiktMatches,
      WikiCa.dabLinkTemplates, WikiCa.needHelpTemplates, WikiCa.helpRequestedTemplates,
      WikiCa.dabList, WikiCa.dabMatches),
  EN( WikiEn.code, WikiEn.name,
      WikiEn.apiUrl, WikiEn.indexUrl,
      WikiEn.helpUrl, WikiEn.helpLink, WikiEn.orientation,
      WikiEn.logged, WikiEn.message,
      WikiEn.wikt, WikiEn.wiktMatches,
      WikiEn.dabLinkTemplates, WikiEn.needHelpTemplates, WikiEn.helpRequestedTemplates,
      WikiEn.dabList, WikiEn.dabMatches),
  ES( WikiEs.code, WikiEs.name,
      WikiEs.apiUrl, WikiEs.indexUrl,
      WikiEs.helpUrl, WikiEs.helpLink, WikiEs.orientation,
      WikiEs.logged, WikiEs.message,
      WikiEs.wikt, WikiEs.wiktMatches,
      WikiEs.dabLinkTemplates, WikiEs.needHelpTemplates, WikiEs.helpRequestedTemplates,
      WikiEs.dabList, WikiEs.dabMatches),
  FR( WikiFr.code, WikiFr.name,
      WikiFr.apiUrl, WikiFr.indexUrl,
      WikiFr.helpUrl, WikiFr.helpLink, WikiFr.orientation,
      WikiFr.logged, WikiFr.message,
      WikiFr.wikt, WikiFr.wiktMatches,
      WikiFr.dabLinkTemplates, WikiFr.needHelpTemplates, WikiFr.helpRequestedTemplates,
      WikiFr.dabList, WikiFr.dabMatches),
  HE( WikiHe.code, WikiHe.name,
      WikiHe.apiUrl, WikiHe.indexUrl,
      WikiHe.helpUrl, WikiHe.helpLink, WikiHe.orientation,
      WikiHe.logged, WikiHe.message,
      WikiHe.wikt, WikiHe.wiktMatches,
      WikiHe.dabLinkTemplates, WikiHe.needHelpTemplates, WikiHe.helpRequestedTemplates,
      WikiHe.dabList, WikiHe.dabMatches),
  NL( WikiNl.code, WikiNl.name,
      WikiNl.apiUrl, WikiNl.indexUrl,
      WikiNl.helpUrl, WikiNl.helpLink, WikiNl.orientation,
      WikiNl.logged, WikiNl.message,
      WikiNl.wikt, WikiNl.wiktMatches,
      WikiNl.dabLinkTemplates, WikiNl.needHelpTemplates, WikiNl.helpRequestedTemplates,
      WikiNl.dabList, WikiNl.dabMatches),
  NO( WikiNo.code, WikiNo.name,
      WikiNo.apiUrl, WikiNo.indexUrl,
      WikiNo.helpUrl, WikiNo.helpLink, WikiNo.orientation,
      WikiNo.logged, WikiNo.message,
      WikiNo.wikt, WikiNo.wiktMatches,
      WikiNo.dabLinkTemplates, WikiNo.needHelpTemplates, WikiNo.helpRequestedTemplates,
      WikiNo.dabList, WikiNo.dabMatches),
  PL( WikiPl.code, WikiPl.name,
      WikiPl.apiUrl, WikiPl.indexUrl,
      WikiPl.helpUrl, WikiPl.helpLink, WikiPl.orientation,
      WikiPl.logged, WikiPl.message,
      WikiPl.wikt, WikiPl.wiktMatches,
      WikiPl.dabLinkTemplates, WikiPl.needHelpTemplates, WikiPl.helpRequestedTemplates,
      WikiPl.dabList, WikiPl.dabMatches),
  RU( WikiRu.code, WikiRu.name,
      WikiRu.apiUrl, WikiRu.indexUrl,
      WikiRu.helpUrl, WikiRu.helpLink, WikiRu.orientation,
      WikiRu.logged, WikiRu.message,
      WikiRu.wikt, WikiRu.wiktMatches,
      WikiRu.dabLinkTemplates, WikiRu.needHelpTemplates, WikiRu.helpRequestedTemplates,
      WikiRu.dabList, WikiRu.dabMatches);

  private final String code;
  private final String title;
  private final String apiUrl;
  private final String wikiUrl;
  private final String helpUrl;
  private final String helpPage;
  private final ComponentOrientation componentOrientation;
  private final String loginSuccessfulText;
  private final String disambiguationText;
  private final String wiktionaryInterwiki;
  private final TemplateMatch[] wiktionaryMatches;
  private final String[] templatesForDisambiguationLink;
  private final String[] templatesForNeedingHelp;
  private final String[] templatesForHelpRequested;
  private final String disambiguationList;
  private ArrayList<Page> disambiguationTemplates2;
  private final TemplateMatch[] disambiguationMatches;

  private List<Namespace> namespaces;

  /**
   * @param code Code.
   * @param title Title.
   * @param apiUrl URL of api.php.
   * @param wikiUrl URL of the wiki.
   * @param helpUrl URL of the help page.
   * @param helpPage Help page.
   * @param loginSuccessfullText Text indicating successful login.
   * @param disambiguationText Text indicating disambiguation repairing.
   * @param wiktionaryInterwiki Interwiki link to wiktionary.
   * @param wiktionaryMatches List of templates for wiktionary.
   * @param templatesForDisambiguationLink Template used to indicate a normal link to disambiguation page.
   * @param templatesForNeedingHelp Templates used to indicate a link needed help to fix.
   * @param templatesForHelpRequested Templates used to find pages where help is requested.
   * @param disambiguationList Page containing the list of disambiguation pages to work on.
   * @param templateMatches List of templates to analyze when looking for links.
   */
  EnumWikipedia(
      String code,
      String title,
      String apiUrl,
      String wikiUrl,
      String helpUrl,
      String helpPage,
      ComponentOrientation componentOrientation,
      String loginSuccessfullText,
      String disambiguationText,
      String wiktionaryInterwiki,
      TemplateMatch[] wiktionaryMatches,
      String[] templatesForDisambiguationLink,
      String[] templatesForNeedingHelp,
      String[] templatesForHelpRequested,
      String disambiguationList,
      TemplateMatch[] templateMatches) {
    this.code = code;
    this.title = title;
    this.apiUrl = apiUrl;
    this.wikiUrl = wikiUrl;
    this.helpUrl = helpUrl;
    this.helpPage = helpPage;
    this.componentOrientation = componentOrientation;
    this.loginSuccessfulText = loginSuccessfullText;
    this.disambiguationText = disambiguationText;
    this.wiktionaryInterwiki = wiktionaryInterwiki;
    this.wiktionaryMatches = wiktionaryMatches;
    this.templatesForDisambiguationLink = templatesForDisambiguationLink;
    this.templatesForNeedingHelp = templatesForNeedingHelp;
    this.templatesForHelpRequested = templatesForHelpRequested;
    this.disambiguationList = disambiguationList;
    this.disambiguationMatches = templateMatches;
  }

  /**
   * @return Vector of all Wikipedia.
   */
  public static Vector<EnumWikipedia> getVector() {
    Vector<EnumWikipedia> vector = new Vector<EnumWikipedia>(EnumWikipedia.values().length);
    for (EnumWikipedia e : EnumWikipedia.values()) {
      vector.add(e);
    }
    return vector;
  }

  /**
   * @param code Language code.
   * @return Corresponding Wikipedia.
   */
  public static EnumWikipedia getWikipedia(String code) {
    for (EnumWikipedia e : EnumWikipedia.values()) {
      if (e.getCode().equals(code)) {
        return e;
      }
    }
    return getDefaultWikipedia();
  }

  /**
   * @return Default Wikipedia.
   */
  public static EnumWikipedia getDefaultWikipedia() {
    Locale locale = Locale.getDefault();
    if ((locale != null) && (locale.getLanguage() != null)) {
      for (EnumWikipedia e : EnumWikipedia.values()) {
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
   * @return Title.
   */
  public String getTitle() {
    return title;
  }

  /**
   * @return URL of api.php.
   */
  public String getApiURL() {
    return apiUrl;
  }

  /**
   * @return URL of the wiki.
   */
  public String getWikiURL() {
    return wikiUrl;
  }

  /**
   * @return URL of the help page.
   */
  public String getHelpURL() {
    return helpUrl;
  }

  /**
   * @return Component orientation.
   */
  public ComponentOrientation getComponentOrientation() {
    return componentOrientation;
  }

  /**
   * @return Text indicating successful login.
   */
  public String getLoginSuccessfulString() {
    return loginSuccessfulText;
  }

  /**
   * @return Text indicating disambiguation repairing.
   */
  public String getDisambiguationString() {
    return disambiguationText;
  }

  /**
   * @return Text used for commenting an update on Wikipedia
   */
  public String getUpdatePageMessage() {
    return getDisambiguationString();
  }

  /**
   * @param text Comment.
   * @param details Details about the update.
   * @return Full comment.
   */
  public String createUpdatePageComment(String text, String details) {
    Configuration configuration = Configuration.getConfiguration();
    boolean comment = configuration.getBoolean(
        Configuration.BOOLEAN_WIKICLEANER_COMMENT,
        Configuration.DEFAULT_WIKICLEANER_COMMENT);

    if (comment) {
      return "[[" + helpPage + "|WikiCleaner]] " + Version.VERSION +
             (((text != null) && (text.length() > 0)) ? " - " + text : "") +
             (((details != null) && (details.length() > 0)) ? " - " + details : "");
    }
    return "" +
           (((text != null) && (text.length() > 0)) ? text : "") +
           (((details != null) && (details.length() > 0)) ? " - " + details : "");
  }

  /**
   * @return Wiktionary interwiki.
   */
  public String getWiktionaryInterwiki() {
    return wiktionaryInterwiki;
  }

  /**
   * @param page Page name.
   * @return Flag indicating if <code>page</code> is a wiktionary template.
   */
  public boolean isWiktionaryTemplate(String page) {
    if ((wiktionaryMatches == null) || (page == null)) {
      return false;
    }
    for (TemplateMatch element : wiktionaryMatches) {
      if (page.equals(Namespace.getTitle(Namespace.TEMPLATE, namespaces, element.getName()))) {
        return true;
      }
    }
    return false;
  }
  /**
   * @return Count of wiktionary templates.
   */
  public int getWiktionaryMatchesCount() {
    return (wiktionaryMatches != null) ? wiktionaryMatches.length : 0;
  }

  /**
   * @param index Wiktionary template index.
   * @return Wiktionary template to analyze for links.
   */
  public TemplateMatch getWiktionaryMatch(int index) {
    return wiktionaryMatches[index];
  }

  /**
   * @return Templates used for a normal link to a disambiguation page.
   */
  public String[] getTemplatesForDisambiguationLink() {
    if (templatesForDisambiguationLink != null) {
      return templatesForDisambiguationLink.clone();
    }
    return null;
  }

  /**
   * @return Templates used for a link where help is required.
   */
  public String[] getTemplatesForNeedingHelp() {
    if (templatesForNeedingHelp != null) {
      return templatesForNeedingHelp.clone();
    }
    return null;
  }

  /**
   * @return Templates used for finding pages where help is requested.
   */
  public ArrayList<Page> getTemplatesForHelpRequested() {
    if (templatesForHelpRequested != null) {
      ArrayList<Page> list = new ArrayList<Page>(templatesForHelpRequested.length);
      for (String template : templatesForHelpRequested) {
        list.add(DataManager.getPage(
            this,
            Namespace.getTitle(Namespace.TEMPLATE, namespaces, template),
            null));
      }
      return list;
    }
    return null;
  }

  /**
   * @return Page containing the list of disambiguation pages.
   */
  public String getDisambiguationList() {
    return disambiguationList;
  }

  /**
   * @param pageName Page name.
   * @return Flag indicating if <code>page</code> is a disambiguation template.
   */
  public boolean isDisambiguationTemplate(String pageName, API api) {
    if (disambiguationTemplates2 == null) {
      synchronized(api) {
        Page page = DataManager.getPage(this, "Mediawiki:Disambiguationspage", null);
        try {
          api.retrieveLinks(page);
        } catch (APIException e) {
          // Error retrieving Disambiguation templates list
        }
        disambiguationTemplates2 = page.getLinks();
      }
    }
    if (disambiguationTemplates2 != null) {
      for (Page page : disambiguationTemplates2) {
        if (Page.areSameTitle(page.getTitle(), pageName)) {
          return true;
        }
      }
    }
    return false;
  }

  /**
   * @return Count of disambiguation templates.
   */
  public int getDisambiguationMatchesCount() {
    return (disambiguationMatches != null) ? disambiguationMatches.length : 0;
  }

  /**
   * @param index Disambiguation template index.
   * @return Disambiguation template to analyze for links.
   */
  public TemplateMatch getDisambiguationMatch(int index) {
    return disambiguationMatches[index];
  }

  /**
   * @return List of namespaces
   */
  public List<Namespace> getNamespaces() {
    return namespaces;
  }

  /**
   * @param namespaces List of namespaces
   */
  public void setNamespaces(List<Namespace> namespaces) {
    this.namespaces = namespaces;
  }

  /* (non-Javadoc)
   * @see java.lang.Enum#toString()
   */
  @Override
  public String toString() {
    return getCode() + " - " + getTitle();
  }
}
