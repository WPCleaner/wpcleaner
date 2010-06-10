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
import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Properties;
import java.util.Vector;

import org.wikipediacleaner.Version;
import org.wikipediacleaner.api.base.API;
import org.wikipediacleaner.api.base.APIException;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Interwiki;
import org.wikipediacleaner.api.data.Language;
import org.wikipediacleaner.api.data.MagicWord;
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
   * - Component orientation.
   * 
   * - Internal link to the configuration page.
   * OR
   * - full URL of the help page.
   * - Internal link to the help page.
   * - "Disambiguation correction" text.
   * - Wiktionary interwiki.
   * - Wiktionary templates.
   * - Templates for indicating that a link to a disambiguation page is normal.
   * - Templates for requiring help on a disambiguation (used for marking links).
   * - Templates for requiring help on a disambiguation (used for getting list of pages requiring help).
   * - Disambiguation project page giving the list of pages to work on.
   * - Array of disambiguation templates.
   * - Array of special templates.
   * - Project Check Wikipedia page.
   * - Project Check Wikipedia traduction.
   */

  AF( WikiAf.code, WikiAf.name,
      WikiAf.apiUrl, WikiAf.indexUrl,
      WikiAf.orientation, WikiAf.configuration,
      WikiAf.wikt, WikiAf.wiktMatches,
      WikiAf.dabLinkTemplates, WikiAf.needHelpTemplates, WikiAf.helpRequestedTemplates,
      WikiAf.dabMatches),
  AR( WikiAr.code, WikiAr.name,
      WikiAr.apiUrl, WikiAr.indexUrl,
      WikiAr.orientation, WikiAr.configuration,
      WikiAr.wikt, WikiAr.wiktMatches,
      WikiAr.dabLinkTemplates, WikiAr.needHelpTemplates, WikiAr.helpRequestedTemplates,
      WikiAr.dabMatches),
  CA( WikiCa.code, WikiCa.name,
      WikiCa.apiUrl, WikiCa.indexUrl,
      WikiCa.orientation, WikiCa.configuration,
      WikiCa.wikt, WikiCa.wiktMatches,
      WikiCa.dabLinkTemplates, WikiCa.needHelpTemplates, WikiCa.helpRequestedTemplates,
      WikiCa.dabMatches),
  CS( WikiCs.code, WikiCs.name,
      WikiCs.apiUrl, WikiCs.indexUrl,
      WikiCs.orientation, WikiCs.configuration,
      WikiCs.wikt, WikiCs.wiktMatches,
      WikiCs.dabLinkTemplates, WikiCs.needHelpTemplates, WikiCs.helpRequestedTemplates,
      WikiCs.dabMatches),
  CY( WikiCy.code, WikiCy.name,
      WikiCy.apiUrl, WikiCy.indexUrl,
      WikiCy.orientation, WikiCy.configuration,
      WikiCy.wikt, WikiCy.wiktMatches,
      WikiCy.dabLinkTemplates, WikiCy.needHelpTemplates, WikiCy.helpRequestedTemplates,
      WikiCy.dabMatches),
  DA( WikiDa.code, WikiDa.name,
      WikiDa.apiUrl, WikiDa.indexUrl,
      WikiDa.orientation, WikiDa.configuration,
      WikiDa.wikt, WikiDa.wiktMatches,
      WikiDa.dabLinkTemplates, WikiDa.needHelpTemplates, WikiDa.helpRequestedTemplates,
      WikiDa.dabMatches),
  DE( WikiDe.code, WikiDe.name,
      WikiDe.apiUrl, WikiDe.indexUrl,
      WikiDe.orientation, WikiDe.configuration,
      WikiDe.wikt, WikiDe.wiktMatches,
      WikiDe.dabLinkTemplates, WikiDe.needHelpTemplates, WikiDe.helpRequestedTemplates,
      WikiDe.dabMatches),
  EN( WikiEn.code, WikiEn.name,
      WikiEn.apiUrl, WikiEn.indexUrl,
      WikiEn.orientation, WikiEn.configuration,
      WikiEn.wikt, WikiEn.wiktMatches,
      WikiEn.dabLinkTemplates, WikiEn.needHelpTemplates, WikiEn.helpRequestedTemplates,
      WikiEn.dabMatches),
  EO( WikiEo.code, WikiEo.name,
      WikiEo.apiUrl, WikiEo.indexUrl,
      WikiEo.orientation, WikiEo.configuration,
      WikiEo.wikt, WikiEo.wiktMatches,
      WikiEo.dabLinkTemplates, WikiEo.needHelpTemplates, WikiEo.helpRequestedTemplates,
      WikiEo.dabMatches),
  ES( WikiEs.code, WikiEs.name,
      WikiEs.apiUrl, WikiEs.indexUrl,
      WikiEs.orientation, WikiEs.configuration,
      WikiEs.wikt, WikiEs.wiktMatches,
      WikiEs.dabLinkTemplates, WikiEs.needHelpTemplates, WikiEs.helpRequestedTemplates,
      WikiEs.dabMatches),
  FI( WikiFi.code, WikiFi.name,
      WikiFi.apiUrl, WikiFi.indexUrl,
      WikiFi.orientation, WikiFi.configuration,
      WikiFi.wikt, WikiFi.wiktMatches,
      WikiFi.dabLinkTemplates, WikiFi.needHelpTemplates, WikiFi.helpRequestedTemplates,
      WikiFi.dabMatches),
  FR( WikiFr.code, WikiFr.name,
      WikiFr.apiUrl, WikiFr.indexUrl,
      WikiFr.orientation, WikiFr.configuration,
      WikiFr.wikt, WikiFr.wiktMatches,
      WikiFr.dabLinkTemplates, WikiFr.needHelpTemplates, WikiFr.helpRequestedTemplates,
      WikiFr.dabMatches),
  FY( WikiFy.code, WikiFy.name,
      WikiFy.apiUrl, WikiFy.indexUrl,
      WikiFy.orientation, WikiFy.configuration,
      WikiFy.wikt, WikiFy.wiktMatches,
      WikiFy.dabLinkTemplates, WikiFy.needHelpTemplates, WikiFy.helpRequestedTemplates,
      WikiFy.dabMatches),
  GD( WikiGd.code, WikiGd.name,
      WikiGd.apiUrl, WikiGd.indexUrl,
      WikiGd.orientation, WikiGd.configuration,
      WikiGd.wikt, WikiGd.wiktMatches,
      WikiGd.dabLinkTemplates, WikiGd.needHelpTemplates, WikiGd.helpRequestedTemplates,
      WikiGd.dabMatches),
  HE( WikiHe.code, WikiHe.name,
      WikiHe.apiUrl, WikiHe.indexUrl,
      WikiHe.orientation, WikiHe.configuration,
      WikiHe.wikt, WikiHe.wiktMatches,
      WikiHe.dabLinkTemplates, WikiHe.needHelpTemplates, WikiHe.helpRequestedTemplates,
      WikiHe.dabMatches),
  HU( WikiHu.code, WikiHu.name,
      WikiHu.apiUrl, WikiHu.indexUrl,
      WikiHu.orientation, WikiHu.configuration,
      WikiHu.wikt, WikiHu.wiktMatches,
      WikiHu.dabLinkTemplates, WikiHu.needHelpTemplates, WikiHu.helpRequestedTemplates,
      WikiHu.dabMatches),
  ID( WikiId.code, WikiId.name,
      WikiId.apiUrl, WikiId.indexUrl,
      WikiId.orientation, WikiId.configuration,
      WikiId.wikt, WikiId.wiktMatches,
      WikiId.dabLinkTemplates, WikiId.needHelpTemplates, WikiId.helpRequestedTemplates,
      WikiId.dabMatches),
  IS( WikiIs.code, WikiIs.name,
      WikiIs.apiUrl, WikiIs.indexUrl,
      WikiIs.orientation, WikiIs.configuration,
      WikiIs.wikt, WikiIs.wiktMatches,
      WikiIs.dabLinkTemplates, WikiIs.needHelpTemplates, WikiIs.helpRequestedTemplates,
      WikiIs.dabMatches),
  IT( WikiIt.code, WikiIt.name,
      WikiIt.apiUrl, WikiIt.indexUrl,
      WikiIt.orientation, WikiIt.configuration,
      WikiIt.wikt, WikiIt.wiktMatches,
      WikiIt.dabLinkTemplates, WikiIt.needHelpTemplates, WikiIt.helpRequestedTemplates,
      WikiIt.dabMatches),
  JA( WikiJa.code, WikiJa.name,
      WikiJa.apiUrl, WikiJa.indexUrl,
      WikiJa.orientation, WikiJa.configuration,
      WikiJa.wikt, WikiJa.wiktMatches,
      WikiJa.dabLinkTemplates, WikiJa.needHelpTemplates, WikiJa.helpRequestedTemplates,
      WikiJa.dabMatches),
  LA( WikiLa.code, WikiLa.name,
      WikiLa.apiUrl, WikiLa.indexUrl,
      WikiLa.orientation, WikiLa.configuration,
      WikiLa.wikt, WikiLa.wiktMatches,
      WikiLa.dabLinkTemplates, WikiLa.needHelpTemplates, WikiLa.helpRequestedTemplates,
      WikiLa.dabMatches),
  NDS(WikiNds.code, WikiNds.name,
      WikiNds.apiUrl, WikiNds.indexUrl,
      WikiNds.orientation, WikiNds.configuration,
      WikiNds.wikt, WikiNds.wiktMatches,
      WikiNds.dabLinkTemplates, WikiNds.needHelpTemplates, WikiNds.helpRequestedTemplates,
      WikiNds.dabMatches),
  NDS_NL(WikiNdsNl.code, WikiNdsNl.name,
      WikiNdsNl.apiUrl, WikiNdsNl.indexUrl,
      WikiNdsNl.orientation, WikiNdsNl.configuration,
      WikiNdsNl.wikt, WikiNdsNl.wiktMatches,
      WikiNdsNl.dabLinkTemplates, WikiNdsNl.needHelpTemplates, WikiNdsNl.helpRequestedTemplates,
      WikiNdsNl.dabMatches),
  NL( WikiNl.code, WikiNl.name,
      WikiNl.apiUrl, WikiNl.indexUrl,
      WikiNl.orientation, WikiNl.configuration,
      WikiNl.wikt, WikiNl.wiktMatches,
      WikiNl.dabLinkTemplates, WikiNl.needHelpTemplates, WikiNl.helpRequestedTemplates,
      WikiNl.dabMatches),
  NO( WikiNo.code, WikiNo.name,
      WikiNo.apiUrl, WikiNo.indexUrl,
      WikiNo.orientation, WikiNo.configuration,
      WikiNo.wikt, WikiNo.wiktMatches,
      WikiNo.dabLinkTemplates, WikiNo.needHelpTemplates, WikiNo.helpRequestedTemplates,
      WikiNo.dabMatches),
  PDC(WikiPdc.code, WikiPdc.name,
      WikiPdc.apiUrl, WikiPdc.indexUrl,
      WikiPdc.orientation, WikiPdc.configuration,
      WikiPdc.wikt, WikiPdc.wiktMatches,
      WikiPdc.dabLinkTemplates, WikiPdc.needHelpTemplates, WikiPdc.helpRequestedTemplates,
      WikiPdc.dabMatches),
  PL( WikiPl.code, WikiPl.name,
      WikiPl.apiUrl, WikiPl.indexUrl,
      WikiPl.orientation, WikiPl.configuration,
      WikiPl.wikt, WikiPl.wiktMatches,
      WikiPl.dabLinkTemplates, WikiPl.needHelpTemplates, WikiPl.helpRequestedTemplates,
      WikiPl.dabMatches),
  PT( WikiPt.code, WikiPt.name,
      WikiPt.apiUrl, WikiPt.indexUrl,
      WikiPt.orientation, WikiPt.configuration,
      WikiPt.wikt, WikiPt.wiktMatches,
      WikiPt.dabLinkTemplates, WikiPt.needHelpTemplates, WikiPt.helpRequestedTemplates,
      WikiPt.dabMatches),
  RO( WikiRo.code, WikiRo.name,
      WikiRo.apiUrl, WikiRo.indexUrl,
      WikiRo.orientation, WikiRo.configuration,
      WikiRo.wikt, WikiRo.wiktMatches,
      WikiRo.dabLinkTemplates, WikiRo.needHelpTemplates, WikiRo.helpRequestedTemplates,
      WikiRo.dabMatches),
  RU( WikiRu.code, WikiRu.name,
      WikiRu.apiUrl, WikiRu.indexUrl,
      WikiRu.orientation, WikiRu.configuration,
      WikiRu.wikt, WikiRu.wiktMatches,
      WikiRu.dabLinkTemplates, WikiRu.needHelpTemplates, WikiRu.helpRequestedTemplates,
      WikiRu.dabMatches),
  SK( WikiSk.code, WikiSk.name,
      WikiSk.apiUrl, WikiSk.indexUrl,
      WikiSk.orientation, WikiSk.configuration,
      WikiSk.wikt, WikiSk.wiktMatches,
      WikiSk.dabLinkTemplates, WikiSk.needHelpTemplates, WikiSk.helpRequestedTemplates,
      WikiSk.dabMatches),
  SL( WikiSl.code, WikiSl.name,
      WikiSl.apiUrl, WikiSl.indexUrl,
      WikiSl.orientation, WikiSl.configuration,
      WikiSl.wikt, WikiSl.wiktMatches,
      WikiSl.dabLinkTemplates, WikiSl.needHelpTemplates, WikiSl.helpRequestedTemplates,
      WikiSl.dabMatches),
  SV( WikiSv.code, WikiSv.name,
      WikiSv.apiUrl, WikiSv.indexUrl,
      WikiSv.orientation, WikiSv.configuration,
      WikiSv.wikt, WikiSv.wiktMatches,
      WikiSv.dabLinkTemplates, WikiSv.needHelpTemplates, WikiSv.helpRequestedTemplates,
      WikiSv.dabMatches),
  TR( WikiTr.code, WikiTr.name,
      WikiTr.apiUrl, WikiTr.indexUrl,
      WikiTr.orientation, WikiTr.configuration,
      WikiTr.wikt, WikiTr.wiktMatches,
      WikiTr.dabLinkTemplates, WikiTr.needHelpTemplates, WikiTr.helpRequestedTemplates,
      WikiTr.dabMatches),
  UK( WikiUk.code, WikiUk.name,
      WikiUk.apiUrl, WikiUk.indexUrl,
      WikiUk.orientation, WikiUk.configuration,
      WikiUk.wikt, WikiUk.wiktMatches,
      WikiUk.dabLinkTemplates, WikiUk.needHelpTemplates, WikiUk.helpRequestedTemplates,
      WikiUk.dabMatches),
  YI( WikiYi.code, WikiYi.name,
      WikiYi.apiUrl, WikiYi.indexUrl,
      WikiYi.orientation, WikiYi.configuration,
      WikiYi.wikt, WikiYi.wiktMatches,
      WikiYi.dabLinkTemplates, WikiYi.needHelpTemplates, WikiYi.helpRequestedTemplates,
      WikiYi.dabMatches),
  ZH( WikiZh.code, WikiZh.name,
      WikiZh.apiUrl, WikiZh.indexUrl,
      WikiZh.orientation, WikiZh.configuration,
      WikiZh.wikt, WikiZh.wiktMatches,
      WikiZh.dabLinkTemplates, WikiZh.needHelpTemplates, WikiZh.helpRequestedTemplates,
      WikiZh.dabMatches),
  COMMONS(WikiCommons.code, WikiCommons.name,
          WikiCommons.apiUrl, WikiCommons.indexUrl,
          WikiCommons.orientation, WikiCommons.configuration,
          WikiCommons.wikt, WikiCommons.wiktMatches,
          WikiCommons.dabLinkTemplates, WikiCommons.needHelpTemplates, WikiCommons.helpRequestedTemplates,
          WikiCommons.dabMatches);

  private final String code;
  private final String title;
  private final String apiUrl;
  private final String wikiUrl;
  private final String helpUrl;
  private final String helpPage;
  private final ComponentOrientation componentOrientation;
  private final String configPage;
  private final Properties configuration;
  private final String disambiguationText;
  private final String wiktionaryInterwiki;
  private final TemplateMatch[] wiktionaryMatches;
  private final String[] templatesForDisambiguationLink;
  private final String[] templatesForNeedingHelp;
  private final String[] templatesForHelpRequested;
  private final String[] disambiguationList;
  private ArrayList<Page> disambiguationTemplates;
  private final TemplateMatch[] disambiguationMatches;
  private final String checkWikiProject;
  private final String checkWikiTraduction;

  private List<Namespace> namespaces;
  private List<Language>  languages;
  private List<Interwiki> interwikis;
  private HashMap<String, MagicWord> magicWords;

  /**
   * @param code Code.
   * @param title Title.
   * @param apiUrl URL of api.php.
   * @param wikiUrl URL of the wiki.
   * @param configPage Configuration page.
   * @param wiktionaryInterwiki Interwiki link to wiktionary.
   * @param wiktionaryMatches List of templates for wiktionary.
   * @param templatesForDisambiguationLink Template used to indicate a normal link to disambiguation page.
   * @param templatesForNeedingHelp Templates used to indicate a link needed help to fix.
   * @param templatesForHelpRequested Templates used to find pages where help is requested.
   * @param templateMatches List of templates to analyze when looking for links.
   */
  EnumWikipedia(
      String code,
      String title,
      String apiUrl,
      String wikiUrl,
      ComponentOrientation componentOrientation,
      String configPage,
      String wiktionaryInterwiki,
      TemplateMatch[] wiktionaryMatches,
      String[] templatesForDisambiguationLink,
      String[] templatesForNeedingHelp,
      String[] templatesForHelpRequested,
      TemplateMatch[] templateMatches) {
    this.code = code;
    this.title = title;
    this.apiUrl = apiUrl;
    this.wikiUrl = wikiUrl;
    this.helpUrl = null;
    this.helpPage = null;
    this.configPage = configPage;
    this.configuration = new Properties();
    this.componentOrientation = componentOrientation;
    this.disambiguationText = null;
    this.wiktionaryInterwiki = wiktionaryInterwiki;
    this.wiktionaryMatches = wiktionaryMatches;
    this.templatesForDisambiguationLink = templatesForDisambiguationLink;
    this.templatesForNeedingHelp = templatesForNeedingHelp;
    this.templatesForHelpRequested = templatesForHelpRequested;
    this.disambiguationList = null;
    this.disambiguationMatches = templateMatches;
    this.checkWikiProject = null;
    this.checkWikiTraduction = null;
  }

  /**
   * @param code Code.
   * @param title Title.
   * @param apiUrl URL of api.php.
   * @param wikiUrl URL of the wiki.
   * @param helpUrl URL of the help page.
   * @param helpPage Help page.
   * @param disambiguationText Text indicating disambiguation repairing.
   * @param wiktionaryInterwiki Interwiki link to wiktionary.
   * @param wiktionaryMatches List of templates for wiktionary.
   * @param templatesForDisambiguationLink Template used to indicate a normal link to disambiguation page.
   * @param templatesForNeedingHelp Templates used to indicate a link needed help to fix.
   * @param templatesForHelpRequested Templates used to find pages where help is requested.
   * @param disambiguationList Page(s) containing the list of disambiguation pages to work on.
   * @param templateMatches List of templates to analyze when looking for links.
   * @param checkWikiProject Project Check Wikipedia page.
   * @param checkWikiTraduction Project Check Wikipedia traduction.
   */
  EnumWikipedia(
      String code,
      String title,
      String apiUrl,
      String wikiUrl,
      String helpUrl,
      String helpPage,
      ComponentOrientation componentOrientation,
      String disambiguationText,
      String wiktionaryInterwiki,
      TemplateMatch[] wiktionaryMatches,
      String[] templatesForDisambiguationLink,
      String[] templatesForNeedingHelp,
      String[] templatesForHelpRequested,
      String[] disambiguationList,
      TemplateMatch[] templateMatches,
      String checkWikiProject,
      String checkWikiTraduction) {
    this.code = code;
    this.title = title;
    this.apiUrl = apiUrl;
    this.wikiUrl = wikiUrl;
    this.helpUrl = helpUrl;
    this.helpPage = helpPage;
    this.componentOrientation = componentOrientation;
    this.configPage = null;
    this.configuration = null;
    this.disambiguationText = disambiguationText;
    this.wiktionaryInterwiki = wiktionaryInterwiki;
    this.wiktionaryMatches = wiktionaryMatches;
    this.templatesForDisambiguationLink = templatesForDisambiguationLink;
    this.templatesForNeedingHelp = templatesForNeedingHelp;
    this.templatesForHelpRequested = templatesForHelpRequested;
    this.disambiguationList = disambiguationList;
    this.disambiguationMatches = templateMatches;
    this.checkWikiProject = checkWikiProject;
    this.checkWikiTraduction = checkWikiTraduction;
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
    if (helpUrl != null) {
      return helpUrl;
    }
    if (configuration != null) {
      String tmp = configuration.getProperty("help_url", null);
      if ((tmp != null) && (tmp.trim().length() > 0)) {
        return tmp.trim();
      }
    }
    return "http://en.wikipedia.org/wiki/User:NicoV/Wikipedia_Cleaner/Documentation";
  }

  /**
   * @return Help page.
   */
  public String getHelpPage() {
    if (helpPage != null) {
      return helpPage;
    }
    if (configuration != null) {
      String tmp = configuration.getProperty("help_page", null);
      if ((tmp != null) && (tmp.trim().length() > 0)) {
        return tmp.trim();
      }
    }
    return null;
  }

  /**
   * @return Component orientation.
   */
  public ComponentOrientation getComponentOrientation() {
    return componentOrientation;
  }

  /**
   * @return Configuration page.
   */
  public String getConfiguationPage() {
    return configPage;
  }

  /**
   * @return Text indicating disambiguation repairing.
   */
  public String getDisambiguationString() {
    if (disambiguationText != null) {
      return disambiguationText;
    }
    if (configuration != null) {
      String tmp = configuration.getProperty("dab_comment", null);
      if ((tmp != null) && (tmp.trim().length() > 0)) {
        return tmp.trim();
      }
    }
    return null;
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
    Configuration config = Configuration.getConfiguration();
    boolean comment = config.getBoolean(
        Configuration.BOOLEAN_WIKICLEANER_COMMENT,
        Configuration.DEFAULT_WIKICLEANER_COMMENT);

    if (comment) {
      String link = getHelpPage();
      String wikiCleaner = null;
      if ((link == null) || (link.trim().length() == 0)) {
        wikiCleaner = "WikiCleaner ";
      } else {
        wikiCleaner = "[[" + link + "|WikiCleaner]] ";
      }
      return wikiCleaner + Version.VERSION +
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
            this, Namespace.getTitle(Namespace.TEMPLATE, namespaces, template),
            null, null));
      }
      return list;
    }
    return null;
  }

  /**
   * @return Pages containing the list of disambiguation pages.
   */
  public String[] getDisambiguationList() {
    if (disambiguationList != null) {
      return disambiguationList;
    }
    if (configuration != null) {
      String tmp = configuration.getProperty("dab_list", null);
      if ((tmp != null) && (tmp.trim().length() > 0)) {
        String[] results = tmp.trim().split("\n");
        if ((results != null) && (results.length > 0)) {
          for (int i = 0; i < results.length; i++) {
            results[i] = results[i].trim();
          }
          return results;
        }
      }
    }
    return null;
  }

  /**
   * Extract next parameter from configuration.
   *
   * @param properties Properties to store the next parameter.
   * @param reader Reader for the configuration.
   * @return Next parameter found.
   * @throw IOException.
   */
  private static boolean getNextParameter(
      Properties properties, BufferedReader reader) throws IOException {
    String line;
    while ((line = reader.readLine()) != null) {
      int posEqual = line.indexOf('=');
      if (posEqual > 0) {
        String name = line.substring(0, posEqual);
        line = line.substring(posEqual + 1);
        int posEnd = line.indexOf(" END");
        while (posEnd == -1) {
          String nextLine = reader.readLine();
          if (nextLine != null) {
            line += "\n" + nextLine;
            posEnd = line.indexOf(" END");
          } else {
            posEnd = line.length();
          }
        }
        line = line.substring(0, posEnd);
        properties.setProperty(name.trim(), line);
        return true;
      }
    }
    return false;
  }

  /**
   * @param config Configuration (page contents)
   */
  public void initConfiguration(String config) {
    configuration.clear();
    if (config != null) {
      BufferedReader reader = null;
      try {
        reader = new BufferedReader(new StringReader(config));
        while (getNextParameter(configuration, reader)) {
          //
        }
      } catch (IOException e) {
        //
      }
    }
  }

  /**
   * @param api Wikipédia API.
   */
  public void initDisambiguationTemplates(API api) {
    if (disambiguationTemplates == null) {
      synchronized(api) {
        Page page = DataManager.getPage(
            this, "Mediawiki:Disambiguationspage",
            null, null);
        try {
          api.retrieveLinks(this, page);
        } catch (APIException e) {
          // Error retrieving Disambiguation templates list
        }
        disambiguationTemplates = page.getLinks();
      }
    }
  }
  
  /**
   * @param pageName Page name.
   * @param api Wikipédia API.
   * @return Flag indicating if <code>page</code> is a disambiguation template.
   */
  public boolean isDisambiguationTemplate(String pageName, API api) {
    if (disambiguationTemplates == null) {
      initDisambiguationTemplates(api);
    }
    if (disambiguationTemplates != null) {
      for (Page page : disambiguationTemplates) {
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
   * @param templateTitle Template's title.
   * @return Disambiguation template.
   */
  public TemplateMatch getDisambiguationMatch(String templateTitle) {
    for (TemplateMatch tm : disambiguationMatches) {
      if ((tm != null) && (Page.areSameTitle(tm.getName(), templateTitle))) {
        return tm;
      }
    }
    return null;
  }

  /**
   * @return Check Wikipedia Project page.
   */
  public String getCheckWikiProject() {
    if (checkWikiProject != null) {
      return checkWikiProject;
    }
    if (configuration != null) {
      String tmp = configuration.getProperty("check_wiki_project_page", null);
      if ((tmp != null) && (tmp.trim().length() > 0)) {
        return tmp;
      }
    }
    return null;
  }

  /**
   * @return Check Wikipedia Project traduction.
   */
  public String getCheckWikiTraduction() {
    if (checkWikiTraduction != null) {
      return checkWikiTraduction;
    }
    if (configuration != null) {
      String tmp = configuration.getProperty("check_wiki_translation_page", null);
      if ((tmp != null) && (tmp.trim().length() > 0)) {
        return tmp;
      }
    }
    return null;
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

  /**
   * @return List of languages
   */
  public List<Language> getLanguages() {
    return languages;
  }

  /**
   * @param languages List of languages
   */
  public void setLanguages(List<Language> languages) {
    this.languages = languages;
  }

  /**
   * @return List of inter-wikis
   */
  public List<Interwiki> getInterwikis() {
    return interwikis;
  }

  /**
   * @param interwikis List of inter-wikis
   */
  public void setInterwikis(List<Interwiki> interwikis) {
    this.interwikis = interwikis;
  }

  /**
   * @param name Magic word name.
   * @return Magic word.
   */
  public MagicWord getMagicWord(String name) {
    if ((name == null) || (magicWords == null)) {
      return null;
    }
    return magicWords.get(name); 
  }

  /**
   * @param text Text
   * @return Flag indicating if the text is an alias for a Image Magic Word.
   */
  public boolean isPossibleAliasForImgMagicWord(String text) {
    if ((getMagicWord(MagicWord.IMG_ALT).isPossibleAlias(text)) ||
        (getMagicWord(MagicWord.IMG_BASELINE).isPossibleAlias(text)) ||
        (getMagicWord(MagicWord.IMG_BORDER).isPossibleAlias(text)) ||
        (getMagicWord(MagicWord.IMG_BOTTOM).isPossibleAlias(text)) ||
        (getMagicWord(MagicWord.IMG_CENTER).isPossibleAlias(text)) ||
        (getMagicWord(MagicWord.IMG_FRAMED).isPossibleAlias(text)) ||
        (getMagicWord(MagicWord.IMG_FRAMELESS).isPossibleAlias(text)) ||
        (getMagicWord(MagicWord.IMG_LEFT).isPossibleAlias(text)) ||
        (getMagicWord(MagicWord.IMG_LINK).isPossibleAlias(text)) ||
        (getMagicWord(MagicWord.IMG_MANUAL_THUMB).isPossibleAlias(text, "[0-9]*")) ||
        (getMagicWord(MagicWord.IMG_MIDDLE).isPossibleAlias(text)) ||
        (getMagicWord(MagicWord.IMG_NONE).isPossibleAlias(text)) ||
        (getMagicWord(MagicWord.IMG_PAGE).isPossibleAlias(text)) ||
        (getMagicWord(MagicWord.IMG_RIGHT).isPossibleAlias(text)) ||
        (getMagicWord(MagicWord.IMG_SUB).isPossibleAlias(text)) ||
        (getMagicWord(MagicWord.IMG_SUPER).isPossibleAlias(text)) ||
        (getMagicWord(MagicWord.IMG_TEXT_BOTTOM).isPossibleAlias(text)) ||
        (getMagicWord(MagicWord.IMG_TEXT_TOP).isPossibleAlias(text)) ||
        (getMagicWord(MagicWord.IMG_THUMBNAIL).isPossibleAlias(text)) ||
        (getMagicWord(MagicWord.IMG_TOP).isPossibleAlias(text)) ||
        (getMagicWord(MagicWord.IMG_UPRIGHT).isPossibleAlias(text, "[0-9]*")) ||
        (getMagicWord(MagicWord.IMG_WIDTH).isPossibleAlias(text, "[0-9]*"))) {
      return true;
    }
    return false;
  }

  /**
   * @param magicWords Magic words.
   */
  public void setMagicWords(HashMap<String, MagicWord> magicWords) {
    this.magicWords = magicWords;
  }

  /* (non-Javadoc)
   * @see java.lang.Enum#toString()
   */
  @Override
  public String toString() {
    return getCode() + " - " + getTitle();
  }
}
