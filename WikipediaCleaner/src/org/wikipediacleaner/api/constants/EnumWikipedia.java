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
      WikiAf.helpUrl, WikiAf.helpLink, WikiAf.orientation,
      WikiAf.message,
      WikiAf.wikt, WikiAf.wiktMatches,
      WikiAf.dabLinkTemplates, WikiAf.needHelpTemplates, WikiAf.helpRequestedTemplates,
      WikiAf.dabList, WikiAf.dabMatches,
      WikiAf.checkWikiProject, WikiAf.checkWikiTraduction),
  AR( WikiAr.code, WikiAr.name,
      WikiAr.apiUrl, WikiAr.indexUrl,
      WikiAr.helpUrl, WikiAr.helpLink, WikiAr.orientation,
      WikiAr.message,
      WikiAr.wikt, WikiAr.wiktMatches,
      WikiAr.dabLinkTemplates, WikiAr.needHelpTemplates, WikiAr.helpRequestedTemplates,
      WikiAr.dabList, WikiAr.dabMatches,
      WikiAr.checkWikiProject, WikiAr.checkWikiTraduction),
  CA( WikiCa.code, WikiCa.name,
      WikiCa.apiUrl, WikiCa.indexUrl,
      WikiCa.helpUrl, WikiCa.helpLink, WikiCa.orientation,
      WikiCa.message,
      WikiCa.wikt, WikiCa.wiktMatches,
      WikiCa.dabLinkTemplates, WikiCa.needHelpTemplates, WikiCa.helpRequestedTemplates,
      WikiCa.dabList, WikiCa.dabMatches,
      WikiCa.checkWikiProject, WikiCa.checkWikiTraduction),
  CS( WikiCs.code, WikiCs.name,
      WikiCs.apiUrl, WikiCs.indexUrl,
      WikiCs.helpUrl, WikiCs.helpLink, WikiCs.orientation,
      WikiCs.message,
      WikiCs.wikt, WikiCs.wiktMatches,
      WikiCs.dabLinkTemplates, WikiCs.needHelpTemplates, WikiCs.helpRequestedTemplates,
      WikiCs.dabList, WikiCs.dabMatches,
      WikiCs.checkWikiProject, WikiCs.checkWikiTraduction),
  CY( WikiCy.code, WikiCy.name,
      WikiCy.apiUrl, WikiCy.indexUrl,
      WikiCy.helpUrl, WikiCy.helpLink, WikiCy.orientation,
      WikiCy.message,
      WikiCy.wikt, WikiCy.wiktMatches,
      WikiCy.dabLinkTemplates, WikiCy.needHelpTemplates, WikiCy.helpRequestedTemplates,
      WikiCy.dabList, WikiCy.dabMatches,
      WikiCy.checkWikiProject, WikiCy.checkWikiTraduction),
  DA( WikiDa.code, WikiDa.name,
      WikiDa.apiUrl, WikiDa.indexUrl,
      WikiDa.helpUrl, WikiDa.helpLink, WikiDa.orientation,
      WikiDa.message,
      WikiDa.wikt, WikiDa.wiktMatches,
      WikiDa.dabLinkTemplates, WikiDa.needHelpTemplates, WikiDa.helpRequestedTemplates,
      WikiDa.dabList, WikiDa.dabMatches,
      WikiDa.checkWikiProject, WikiDa.checkWikiTraduction),
  DE( WikiDe.code, WikiDe.name,
      WikiDe.apiUrl, WikiDe.indexUrl,
      WikiDe.helpUrl, WikiDe.helpLink, WikiDe.orientation,
      WikiDe.message,
      WikiDe.wikt, WikiDe.wiktMatches,
      WikiDe.dabLinkTemplates, WikiDe.needHelpTemplates, WikiDe.helpRequestedTemplates,
      WikiDe.dabList, WikiDe.dabMatches,
      WikiDe.checkWikiProject, WikiDe.checkWikiTraduction),
  EN( WikiEn.code, WikiEn.name,
      WikiEn.apiUrl, WikiEn.indexUrl,
      WikiEn.helpUrl, WikiEn.helpLink, WikiEn.orientation,
      WikiEn.message,
      WikiEn.wikt, WikiEn.wiktMatches,
      WikiEn.dabLinkTemplates, WikiEn.needHelpTemplates, WikiEn.helpRequestedTemplates,
      WikiEn.dabList, WikiEn.dabMatches,
      WikiEn.checkWikiProject, WikiEn.checkWikiTraduction),
  EO( WikiEo.code, WikiEo.name,
      WikiEo.apiUrl, WikiEo.indexUrl,
      WikiEo.helpUrl, WikiEo.helpLink, WikiEo.orientation,
      WikiEo.message,
      WikiEo.wikt, WikiEo.wiktMatches,
      WikiEo.dabLinkTemplates, WikiEo.needHelpTemplates, WikiEo.helpRequestedTemplates,
      WikiEo.dabList, WikiEo.dabMatches,
      WikiEo.checkWikiProject, WikiEo.checkWikiTraduction),
  ES( WikiEs.code, WikiEs.name,
      WikiEs.apiUrl, WikiEs.indexUrl,
      WikiEs.helpUrl, WikiEs.helpLink, WikiEs.orientation,
      WikiEs.message,
      WikiEs.wikt, WikiEs.wiktMatches,
      WikiEs.dabLinkTemplates, WikiEs.needHelpTemplates, WikiEs.helpRequestedTemplates,
      WikiEs.dabList, WikiEs.dabMatches,
      WikiEs.checkWikiProject, WikiEs.checkWikiTraduction),
  FI( WikiFi.code, WikiFi.name,
      WikiFi.apiUrl, WikiFi.indexUrl,
      WikiFi.helpUrl, WikiFi.helpLink, WikiFi.orientation,
      WikiFi.message,
      WikiFi.wikt, WikiFi.wiktMatches,
      WikiFi.dabLinkTemplates, WikiFi.needHelpTemplates, WikiFi.helpRequestedTemplates,
      WikiFi.dabList, WikiFi.dabMatches,
      WikiFi.checkWikiProject, WikiFi.checkWikiTraduction),
  FR( WikiFr.code, WikiFr.name,
      WikiFr.apiUrl, WikiFr.indexUrl,
      WikiFr.orientation, WikiFr.configuration,
      WikiFr.wikt, WikiFr.wiktMatches,
      WikiFr.dabLinkTemplates, WikiFr.needHelpTemplates, WikiFr.helpRequestedTemplates,
      WikiFr.dabList, WikiFr.dabMatches),
  FY( WikiFy.code, WikiFy.name,
      WikiFy.apiUrl, WikiFy.indexUrl,
      WikiFy.helpUrl, WikiFy.helpLink, WikiFy.orientation,
      WikiFy.message,
      WikiFy.wikt, WikiFy.wiktMatches,
      WikiFy.dabLinkTemplates, WikiFy.needHelpTemplates, WikiFy.helpRequestedTemplates,
      WikiFy.dabList, WikiFy.dabMatches,
      WikiFy.checkWikiProject, WikiFy.checkWikiTraduction),
  GD( WikiGd.code, WikiGd.name,
      WikiGd.apiUrl, WikiGd.indexUrl,
      WikiGd.helpUrl, WikiGd.helpLink, WikiGd.orientation,
      WikiGd.message,
      WikiGd.wikt, WikiGd.wiktMatches,
      WikiGd.dabLinkTemplates, WikiGd.needHelpTemplates, WikiGd.helpRequestedTemplates,
      WikiGd.dabList, WikiGd.dabMatches,
      WikiGd.checkWikiProject, WikiGd.checkWikiTraduction),
  HE( WikiHe.code, WikiHe.name,
      WikiHe.apiUrl, WikiHe.indexUrl,
      WikiHe.helpUrl, WikiHe.helpLink, WikiHe.orientation,
      WikiHe.message,
      WikiHe.wikt, WikiHe.wiktMatches,
      WikiHe.dabLinkTemplates, WikiHe.needHelpTemplates, WikiHe.helpRequestedTemplates,
      WikiHe.dabList, WikiHe.dabMatches,
      WikiHe.checkWikiProject, WikiHe.checkWikiTraduction),
  HU( WikiHu.code, WikiHu.name,
      WikiHu.apiUrl, WikiHu.indexUrl,
      WikiHu.helpUrl, WikiHu.helpLink, WikiHu.orientation,
      WikiHu.message,
      WikiHu.wikt, WikiHu.wiktMatches,
      WikiHu.dabLinkTemplates, WikiHu.needHelpTemplates, WikiHu.helpRequestedTemplates,
      WikiHu.dabList, WikiHu.dabMatches,
      WikiHu.checkWikiProject, WikiHu.checkWikiTraduction),
  ID( WikiId.code, WikiId.name,
      WikiId.apiUrl, WikiId.indexUrl,
      WikiId.helpUrl, WikiId.helpLink, WikiId.orientation,
      WikiId.message,
      WikiId.wikt, WikiId.wiktMatches,
      WikiId.dabLinkTemplates, WikiId.needHelpTemplates, WikiId.helpRequestedTemplates,
      WikiId.dabList, WikiId.dabMatches,
      WikiId.checkWikiProject, WikiId.checkWikiTraduction),
  IS( WikiIs.code, WikiIs.name,
      WikiIs.apiUrl, WikiIs.indexUrl,
      WikiIs.helpUrl, WikiIs.helpLink, WikiIs.orientation,
      WikiIs.message,
      WikiIs.wikt, WikiIs.wiktMatches,
      WikiIs.dabLinkTemplates, WikiIs.needHelpTemplates, WikiIs.helpRequestedTemplates,
      WikiIs.dabList, WikiIs.dabMatches,
      WikiIs.checkWikiProject, WikiIs.checkWikiTraduction),
  IT( WikiIt.code, WikiIt.name,
      WikiIt.apiUrl, WikiIt.indexUrl,
      WikiIt.helpUrl, WikiIt.helpLink, WikiIt.orientation,
      WikiIt.message,
      WikiIt.wikt, WikiIt.wiktMatches,
      WikiIt.dabLinkTemplates, WikiIt.needHelpTemplates, WikiIt.helpRequestedTemplates,
      WikiIt.dabList, WikiIt.dabMatches,
      WikiIt.checkWikiProject, WikiIt.checkWikiTraduction),
  JA( WikiJa.code, WikiJa.name,
      WikiJa.apiUrl, WikiJa.indexUrl,
      WikiJa.helpUrl, WikiJa.helpLink, WikiJa.orientation,
      WikiJa.message,
      WikiJa.wikt, WikiJa.wiktMatches,
      WikiJa.dabLinkTemplates, WikiJa.needHelpTemplates, WikiJa.helpRequestedTemplates,
      WikiJa.dabList, WikiJa.dabMatches,
      WikiJa.checkWikiProject, WikiJa.checkWikiTraduction),
  LA( WikiLa.code, WikiLa.name,
      WikiLa.apiUrl, WikiLa.indexUrl,
      WikiLa.helpUrl, WikiLa.helpLink, WikiLa.orientation,
      WikiLa.message,
      WikiLa.wikt, WikiLa.wiktMatches,
      WikiLa.dabLinkTemplates, WikiLa.needHelpTemplates, WikiLa.helpRequestedTemplates,
      WikiLa.dabList, WikiLa.dabMatches,
      WikiLa.checkWikiProject, WikiLa.checkWikiTraduction),
  NDS(WikiNds.code, WikiNds.name,
      WikiNds.apiUrl, WikiNds.indexUrl,
      WikiNds.helpUrl, WikiNds.helpLink, WikiNds.orientation,
      WikiNds.message,
      WikiNds.wikt, WikiNds.wiktMatches,
      WikiNds.dabLinkTemplates, WikiNds.needHelpTemplates, WikiNds.helpRequestedTemplates,
      WikiNds.dabList, WikiNds.dabMatches,
      WikiNds.checkWikiProject, WikiNds.checkWikiTraduction),
  NDS_NL(WikiNdsNl.code, WikiNdsNl.name,
      WikiNdsNl.apiUrl, WikiNdsNl.indexUrl,
      WikiNdsNl.helpUrl, WikiNdsNl.helpLink, WikiNdsNl.orientation,
      WikiNdsNl.message,
      WikiNdsNl.wikt, WikiNdsNl.wiktMatches,
      WikiNdsNl.dabLinkTemplates, WikiNdsNl.needHelpTemplates, WikiNdsNl.helpRequestedTemplates,
      WikiNdsNl.dabList, WikiNdsNl.dabMatches,
      WikiNdsNl.checkWikiProject, WikiNdsNl.checkWikiTraduction),
  NL( WikiNl.code, WikiNl.name,
      WikiNl.apiUrl, WikiNl.indexUrl,
      WikiNl.helpUrl, WikiNl.helpLink, WikiNl.orientation,
      WikiNl.message,
      WikiNl.wikt, WikiNl.wiktMatches,
      WikiNl.dabLinkTemplates, WikiNl.needHelpTemplates, WikiNl.helpRequestedTemplates,
      WikiNl.dabList, WikiNl.dabMatches,
      WikiNl.checkWikiProject, WikiNl.checkWikiTraduction),
  NO( WikiNo.code, WikiNo.name,
      WikiNo.apiUrl, WikiNo.indexUrl,
      WikiNo.helpUrl, WikiNo.helpLink, WikiNo.orientation,
      WikiNo.message,
      WikiNo.wikt, WikiNo.wiktMatches,
      WikiNo.dabLinkTemplates, WikiNo.needHelpTemplates, WikiNo.helpRequestedTemplates,
      WikiNo.dabList, WikiNo.dabMatches,
      WikiNo.checkWikiProject, WikiNo.checkWikiTraduction),
  PDC(WikiPdc.code, WikiPdc.name,
      WikiPdc.apiUrl, WikiPdc.indexUrl,
      WikiPdc.helpUrl, WikiPdc.helpLink, WikiPdc.orientation,
      WikiPdc.message,
      WikiPdc.wikt, WikiPdc.wiktMatches,
      WikiPdc.dabLinkTemplates, WikiPdc.needHelpTemplates, WikiPdc.helpRequestedTemplates,
      WikiPdc.dabList, WikiPdc.dabMatches,
      WikiPdc.checkWikiProject, WikiPdc.checkWikiTraduction),
  PL( WikiPl.code, WikiPl.name,
      WikiPl.apiUrl, WikiPl.indexUrl,
      WikiPl.helpUrl, WikiPl.helpLink, WikiPl.orientation,
      WikiPl.message,
      WikiPl.wikt, WikiPl.wiktMatches,
      WikiPl.dabLinkTemplates, WikiPl.needHelpTemplates, WikiPl.helpRequestedTemplates,
      WikiPl.dabList, WikiPl.dabMatches,
      WikiPl.checkWikiProject, WikiPl.checkWikiTraduction),
  PT( WikiPt.code, WikiPt.name,
      WikiPt.apiUrl, WikiPt.indexUrl,
      WikiPt.helpUrl, WikiPt.helpLink, WikiPt.orientation,
      WikiPt.message,
      WikiPt.wikt, WikiPt.wiktMatches,
      WikiPt.dabLinkTemplates, WikiPt.needHelpTemplates, WikiPt.helpRequestedTemplates,
      WikiPt.dabList, WikiPt.dabMatches,
      WikiPt.checkWikiProject, WikiPt.checkWikiTraduction),
  RO( WikiRo.code, WikiRo.name,
      WikiRo.apiUrl, WikiRo.indexUrl,
      WikiRo.helpUrl, WikiRo.helpLink, WikiRo.orientation,
      WikiRo.message,
      WikiRo.wikt, WikiRo.wiktMatches,
      WikiRo.dabLinkTemplates, WikiRo.needHelpTemplates, WikiRo.helpRequestedTemplates,
      WikiRo.dabList, WikiRo.dabMatches,
      WikiRo.checkWikiProject, WikiRo.checkWikiTraduction),
  RU( WikiRu.code, WikiRu.name,
      WikiRu.apiUrl, WikiRu.indexUrl,
      WikiRu.helpUrl, WikiRu.helpLink, WikiRu.orientation,
      WikiRu.message,
      WikiRu.wikt, WikiRu.wiktMatches,
      WikiRu.dabLinkTemplates, WikiRu.needHelpTemplates, WikiRu.helpRequestedTemplates,
      WikiRu.dabList, WikiRu.dabMatches,
      WikiRu.checkWikiProject, WikiRu.checkWikiTraduction),
  SK( WikiSk.code, WikiSk.name,
      WikiSk.apiUrl, WikiSk.indexUrl,
      WikiSk.helpUrl, WikiSk.helpLink, WikiSk.orientation,
      WikiSk.message,
      WikiSk.wikt, WikiSk.wiktMatches,
      WikiSk.dabLinkTemplates, WikiSk.needHelpTemplates, WikiSk.helpRequestedTemplates,
      WikiSk.dabList, WikiSk.dabMatches,
      WikiSk.checkWikiProject, WikiSk.checkWikiTraduction),
  SL( WikiSl.code, WikiSl.name,
      WikiSl.apiUrl, WikiSl.indexUrl,
      WikiSl.helpUrl, WikiSl.helpLink, WikiSl.orientation,
      WikiSl.message,
      WikiSl.wikt, WikiSl.wiktMatches,
      WikiSl.dabLinkTemplates, WikiSl.needHelpTemplates, WikiSl.helpRequestedTemplates,
      WikiSl.dabList, WikiSl.dabMatches,
      WikiSl.checkWikiProject, WikiSl.checkWikiTraduction),
  SV( WikiSv.code, WikiSv.name,
      WikiSv.apiUrl, WikiSv.indexUrl,
      WikiSv.helpUrl, WikiSv.helpLink, WikiSv.orientation,
      WikiSv.message,
      WikiSv.wikt, WikiSv.wiktMatches,
      WikiSv.dabLinkTemplates, WikiSv.needHelpTemplates, WikiSv.helpRequestedTemplates,
      WikiSv.dabList, WikiSv.dabMatches,
      WikiSv.checkWikiProject, WikiSv.checkWikiTraduction),
  TR( WikiTr.code, WikiTr.name,
      WikiTr.apiUrl, WikiTr.indexUrl,
      WikiTr.helpUrl, WikiTr.helpLink, WikiTr.orientation,
      WikiTr.message,
      WikiTr.wikt, WikiTr.wiktMatches,
      WikiTr.dabLinkTemplates, WikiTr.needHelpTemplates, WikiTr.helpRequestedTemplates,
      WikiTr.dabList, WikiTr.dabMatches,
      WikiTr.checkWikiProject, WikiTr.checkWikiTraduction),
  UK( WikiUk.code, WikiUk.name,
      WikiUk.apiUrl, WikiUk.indexUrl,
      WikiUk.helpUrl, WikiUk.helpLink, WikiUk.orientation,
      WikiUk.message,
      WikiUk.wikt, WikiUk.wiktMatches,
      WikiUk.dabLinkTemplates, WikiUk.needHelpTemplates, WikiUk.helpRequestedTemplates,
      WikiUk.dabList, WikiUk.dabMatches,
      WikiUk.checkWikiProject, WikiUk.checkWikiTraduction),
  YI( WikiYi.code, WikiYi.name,
      WikiYi.apiUrl, WikiYi.indexUrl,
      WikiYi.helpUrl, WikiYi.helpLink, WikiYi.orientation,
      WikiYi.message,
      WikiYi.wikt, WikiYi.wiktMatches,
      WikiYi.dabLinkTemplates, WikiYi.needHelpTemplates, WikiYi.helpRequestedTemplates,
      WikiYi.dabList, WikiYi.dabMatches,
      WikiYi.checkWikiProject, WikiYi.checkWikiTraduction),
  ZH( WikiZh.code, WikiZh.name,
      WikiZh.apiUrl, WikiZh.indexUrl,
      WikiZh.helpUrl, WikiZh.helpLink, WikiZh.orientation,
      WikiZh.message,
      WikiZh.wikt, WikiZh.wiktMatches,
      WikiZh.dabLinkTemplates, WikiZh.needHelpTemplates, WikiZh.helpRequestedTemplates,
      WikiZh.dabList, WikiZh.dabMatches,
      WikiZh.checkWikiProject, WikiZh.checkWikiTraduction);

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
   * @param disambiguationText Text indicating disambiguation repairing.
   * @param wiktionaryInterwiki Interwiki link to wiktionary.
   * @param wiktionaryMatches List of templates for wiktionary.
   * @param templatesForDisambiguationLink Template used to indicate a normal link to disambiguation page.
   * @param templatesForNeedingHelp Templates used to indicate a link needed help to fix.
   * @param templatesForHelpRequested Templates used to find pages where help is requested.
   * @param disambiguationList Page(s) containing the list of disambiguation pages to work on.
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
      String[] disambiguationList,
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
    this.disambiguationList = disambiguationList;
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
      return configuration.getProperty("help_url", null);
    }
    return null;
  }

  /**
   * @return Help page.
   */
  public String getHelpPage() {
    if (helpPage != null) {
      return helpPage;
    }
    if (configuration != null) {
      return configuration.getProperty("help_page", null);
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
      return configuration.getProperty("dab_comment", null);
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
    return disambiguationList;
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
      return configuration.getProperty("check_wiki_project_page", null);
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
      return configuration.getProperty("check_wiki_translation_page", null);
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
