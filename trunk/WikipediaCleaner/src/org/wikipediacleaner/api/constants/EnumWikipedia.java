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
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;

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
import org.wikipediacleaner.api.data.TemplateMatcher;
import org.wikipediacleaner.api.data.TemplateMatcher1L2T;
import org.wikipediacleaner.api.data.TemplateMatcher1LT;
import org.wikipediacleaner.api.data.TemplateMatcher1L;
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
      WikiAf.orientation, WikiAf.configuration),
  AR( WikiAr.code, WikiAr.name,
      WikiAr.apiUrl, WikiAr.indexUrl,
      WikiAr.orientation, WikiAr.configuration),
  BAR(WikiBar.code, WikiBar.name,
      WikiBar.apiUrl, WikiBar.indexUrl,
      WikiBar.orientation, WikiBar.configuration),
  CA( WikiCa.code, WikiCa.name,
      WikiCa.apiUrl, WikiCa.indexUrl,
      WikiCa.orientation, WikiCa.configuration),
  CS( WikiCs.code, WikiCs.name,
      WikiCs.apiUrl, WikiCs.indexUrl,
      WikiCs.orientation, WikiCs.configuration),
  CY( WikiCy.code, WikiCy.name,
      WikiCy.apiUrl, WikiCy.indexUrl,
      WikiCy.orientation, WikiCy.configuration),
  DA( WikiDa.code, WikiDa.name,
      WikiDa.apiUrl, WikiDa.indexUrl,
      WikiDa.orientation, WikiDa.configuration),
  DE( WikiDe.code, WikiDe.name,
      WikiDe.apiUrl, WikiDe.indexUrl,
      WikiDe.orientation, WikiDe.configuration),
  EN( WikiEn.code, WikiEn.name,
      WikiEn.apiUrl, WikiEn.indexUrl,
      WikiEn.orientation, WikiEn.configuration),
  EO( WikiEo.code, WikiEo.name,
      WikiEo.apiUrl, WikiEo.indexUrl,
      WikiEo.orientation, WikiEo.configuration),
  ES( WikiEs.code, WikiEs.name,
      WikiEs.apiUrl, WikiEs.indexUrl,
      WikiEs.orientation, WikiEs.configuration),
  FI( WikiFi.code, WikiFi.name,
      WikiFi.apiUrl, WikiFi.indexUrl,
      WikiFi.orientation, WikiFi.configuration),
  FR( WikiFr.code, WikiFr.name,
      WikiFr.apiUrl, WikiFr.indexUrl,
      WikiFr.orientation, WikiFr.configuration),
  FY( WikiFy.code, WikiFy.name,
      WikiFy.apiUrl, WikiFy.indexUrl,
      WikiFy.orientation, WikiFy.configuration),
  GD( WikiGd.code, WikiGd.name,
      WikiGd.apiUrl, WikiGd.indexUrl,
      WikiGd.orientation, WikiGd.configuration),
  HE( WikiHe.code, WikiHe.name,
      WikiHe.apiUrl, WikiHe.indexUrl,
      WikiHe.orientation, WikiHe.configuration),
  HU( WikiHu.code, WikiHu.name,
      WikiHu.apiUrl, WikiHu.indexUrl,
      WikiHu.orientation, WikiHu.configuration),
  ID( WikiId.code, WikiId.name,
      WikiId.apiUrl, WikiId.indexUrl,
      WikiId.orientation, WikiId.configuration),
  IS( WikiIs.code, WikiIs.name,
      WikiIs.apiUrl, WikiIs.indexUrl,
      WikiIs.orientation, WikiIs.configuration),
  IT( WikiIt.code, WikiIt.name,
      WikiIt.apiUrl, WikiIt.indexUrl,
      WikiIt.orientation, WikiIt.configuration),
  JA( WikiJa.code, WikiJa.name,
      WikiJa.apiUrl, WikiJa.indexUrl,
      WikiJa.orientation, WikiJa.configuration),
  LA( WikiLa.code, WikiLa.name,
      WikiLa.apiUrl, WikiLa.indexUrl,
      WikiLa.orientation, WikiLa.configuration),
  NDS(WikiNds.code, WikiNds.name,
      WikiNds.apiUrl, WikiNds.indexUrl,
      WikiNds.orientation, WikiNds.configuration),
  NDS_NL(WikiNdsNl.code, WikiNdsNl.name,
      WikiNdsNl.apiUrl, WikiNdsNl.indexUrl,
      WikiNdsNl.orientation, WikiNdsNl.configuration),
  NL( WikiNl.code, WikiNl.name,
      WikiNl.apiUrl, WikiNl.indexUrl,
      WikiNl.orientation, WikiNl.configuration),
  NO( WikiNo.code, WikiNo.name,
      WikiNo.apiUrl, WikiNo.indexUrl,
      WikiNo.orientation, WikiNo.configuration),
  PDC(WikiPdc.code, WikiPdc.name,
      WikiPdc.apiUrl, WikiPdc.indexUrl,
      WikiPdc.orientation, WikiPdc.configuration),
  PL( WikiPl.code, WikiPl.name,
      WikiPl.apiUrl, WikiPl.indexUrl,
      WikiPl.orientation, WikiPl.configuration),
  PT( WikiPt.code, WikiPt.name,
      WikiPt.apiUrl, WikiPt.indexUrl,
      WikiPt.orientation, WikiPt.configuration),
  RO( WikiRo.code, WikiRo.name,
      WikiRo.apiUrl, WikiRo.indexUrl,
      WikiRo.orientation, WikiRo.configuration),
  RU( WikiRu.code, WikiRu.name,
      WikiRu.apiUrl, WikiRu.indexUrl,
      WikiRu.orientation, WikiRu.configuration),
  SK( WikiSk.code, WikiSk.name,
      WikiSk.apiUrl, WikiSk.indexUrl,
      WikiSk.orientation, WikiSk.configuration),
  SL( WikiSl.code, WikiSl.name,
      WikiSl.apiUrl, WikiSl.indexUrl,
      WikiSl.orientation, WikiSl.configuration),
  SV( WikiSv.code, WikiSv.name,
      WikiSv.apiUrl, WikiSv.indexUrl,
      WikiSv.orientation, WikiSv.configuration),
  TR( WikiTr.code, WikiTr.name,
      WikiTr.apiUrl, WikiTr.indexUrl,
      WikiTr.orientation, WikiTr.configuration),
  UK( WikiUk.code, WikiUk.name,
      WikiUk.apiUrl, WikiUk.indexUrl,
      WikiUk.orientation, WikiUk.configuration),
  YI( WikiYi.code, WikiYi.name,
      WikiYi.apiUrl, WikiYi.indexUrl,
      WikiYi.orientation, WikiYi.configuration),
  ZH( WikiZh.code, WikiZh.name,
      WikiZh.apiUrl, WikiZh.indexUrl,
      WikiZh.orientation, WikiZh.configuration),
  COMMONS(WikiCommons.code, WikiCommons.name,
          WikiCommons.apiUrl, WikiCommons.indexUrl,
          WikiCommons.orientation, WikiCommons.configuration);

  private final String code;
  private final String title;
  private final String apiUrl;
  private final String wikiUrl;
  private final ComponentOrientation componentOrientation;
  private final Properties configuration;

  private String helpUrl;
  private String helpPage;

  private String pipeTemplate;
  private List<Integer> encyclopedicNamespaces;
  private List<Integer> encyclopedicTalkNamespaces;
  private List<String> todoTemplates;
  private List<String> todoLinkTemplates;
  private String todoSubpage;
  private boolean todoSubpageForce;
  private boolean todoSubpageForceOther;
  private String disambiguationWarningTemplate;
  private String disambiguationWarningTemplateComment;
  private String disambiguationWarningComment;
  private String disambiguationWarningCommentDone;
  private List<String> disambiguationWarningAfterTemplates;
  private Map<String, List<TemplateMatcher>> templateMatchers;

  private final String configPage;
  private String disambiguationText;
  private String wiktionaryInterwiki;
  private TemplateMatch[] wiktionaryMatches;
  private String[] templatesForDisambiguationLink;
  private String[] templatesForNeedingHelp;
  private String[] templatesForHelpRequested;
  private String[] templatesForLinkingText;
  private List<String> disambiguationList;
  private List<Page> disambiguationTemplates;
  private String checkWikiProject;
  private String checkWikiTranslation;

  private List<Namespace> namespaces;
  private List<Language>  languages;
  private List<Interwiki> interwikis;
  private Map<String, MagicWord> magicWords;
  private Properties      checkWikiGeneralConfig;
  private Properties      checkWikiConfig;

  /**
   * @param code Code.
   * @param title Title.
   * @param apiUrl URL of api.php.
   * @param wikiUrl URL of the wiki.
   * @param configPage Configuration page.
   */
  EnumWikipedia(
      String code,
      String title,
      String apiUrl,
      String wikiUrl,
      ComponentOrientation componentOrientation,
      String configPage) {
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
    this.wiktionaryInterwiki = null;
    this.wiktionaryMatches = null;
    this.templatesForDisambiguationLink = null;
    this.templatesForNeedingHelp = null;
    this.templatesForHelpRequested = null;
    this.templatesForLinkingText = null;
    this.disambiguationList = null;
    this.checkWikiProject = null;
    this.checkWikiTranslation = null;
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
    if (disambiguationList != null) {
      this.disambiguationList = new ArrayList<String>();
      for (int i = 0; i < disambiguationList.length; i++) {
        this.disambiguationList.add(disambiguationList[i]);
      }
    }
    this.checkWikiProject = checkWikiProject;
    this.checkWikiTranslation = checkWikiTraduction;
  }

  /**
   * @return Vector of all Wikipedia.
   */
  public static List<EnumWikipedia> getList() {
    List<EnumWikipedia> list = new ArrayList<EnumWikipedia>(EnumWikipedia.values().length);
    for (EnumWikipedia e : EnumWikipedia.values()) {
      list.add(e);
    }
    return list;
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
   * @param pageTitle Title.
   * @return Normalized title.
   */
  public String normalizeTitle(String pageTitle) {
    if (pageTitle == null) {
      return null;
    }
    String result = pageTitle.trim();
    result = result.replaceAll("_", " ");
    result = result.replaceAll(" +", " ");
    result = result.trim();
    result = Page.getStringUcFirst(result);
    return result;
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
   * @param pageTitle Title.
   * @param redirect Follow redirect ?
   * @return URL of the wiki.
   */
  public String getWikiURL(String pageTitle, boolean redirect) {
    try {
      return getWikiURL() +
             "?title=" + URLEncoder.encode(pageTitle, "UTF-8") +
             (redirect ? "" : "&redirect=no");
    } catch (UnsupportedEncodingException e) {
      return getWikiURL();
    }
  }

  /**
   * @param pageTitle Title.
   * @param action Action.
   * @return URL of the wiki.
   */
  public String getWikiURL(String pageTitle, String action) {
    try {
      return getWikiURL() +
             "?title=" + URLEncoder.encode(pageTitle, "UTF-8") +
             "&redirect=no" +
             "&action=" + action;
    } catch (UnsupportedEncodingException e) {
      return getWikiURL();
    }
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
   * @return URL of the help page.
   */
  public String getHelpURL() {
    if (helpUrl != null) {
      return helpUrl;
    }
    return "http://en.wikipedia.org/wiki/User:NicoV/Wikipedia_Cleaner/Documentation";
  }

  /**
   * @return Help page.
   */
  public String getHelpPage() {
    return helpPage;
  }

  /**
   * @return Template creating a "|".
   */
  public String getPipeTemplate() {
    return pipeTemplate;
  }

  /**
   * @return Encyclopedic namespaces.
   */
  public List<Integer> getEncyclopedicNamespaces() {
    return encyclopedicNamespaces;
  }

  /**
   * @param namespace Namespace.
   * @return True if the namespace is encyclopedic.
   */
  public boolean isEncyclopedicNamespace(Integer namespace) {
    if ((encyclopedicNamespaces == null) || (namespace == null)) {
      return false;
    }
    for (Integer tmp : encyclopedicNamespaces) {
      if (namespace.equals(tmp)) {
        return true;
      }
    }
    return false;
  }

  /**
   * @return Encyclopedic talk namespaces.
   */
  public List<Integer> getEncyclopedicTalkNamespaces() {
    return encyclopedicTalkNamespaces;
  }

  /**
   * @return Templates creating todo lists.
   */
  public List<String> getTodoTemplates() {
    return todoTemplates;
  }

  /**
   * @return Templates creating links to todo lists.
   */
  public List<String> getTodoLinkTemplates() {
    return todoLinkTemplates;
  }

  /**
   * @return Todo subpage.
   */
  public String getTodoSubpage() {
    return todoSubpage;
  }

  /**
   * @return Force usage of todo subpage in main namespace.
   */
  public boolean getTodoSubpageForce() {
    return todoSubpageForce;
  }

  /**
   * @return Force usage of todo subpage in other namespaces.
   */
  public boolean getTodoSubpageForceOther() {
    return todoSubpageForceOther;
  }

  /**
   * @return Template for warning about disambiguation links in a page.
   */
  public String getDisambiguationWarningTemplate() {
    return disambiguationWarningTemplate;
  }

  /**
   * @return Comment for warning template about disambiguation links in a page.
   */
  public String getDisambiguationWarningTemplateComment() {
    return disambiguationWarningTemplateComment;
  }

  /**
   * @return Comment for warning about disambiguation links in a page.
   */
  public String getDisambiguationWarningComment() {
    if (disambiguationWarningComment != null) {
      return disambiguationWarningComment;
    }
    return disambiguationWarningTemplate;
  }

  /**
   * @return Comment for telling that disambiguation links have been fixed.
   */
  public String getDisambiguationWarningCommentDone() {
    if (disambiguationWarningCommentDone != null) {
      return disambiguationWarningCommentDone;
    }
    return disambiguationWarningTemplate;
  }

  /**
   * @return List of templates that should be before the disambiguation warning.
   */
  public List<String> getDisambiguationWarningAfterTemplates() {
    return disambiguationWarningAfterTemplates;
  }

  /**
   * @return Some matchers exist ?
   */
  public boolean hasTemplateMatchers() {
    if (templateMatchers == null) {
      return false;
    }
    if (templateMatchers.isEmpty()) {
      return false;
    }
    return true;
  }

  /**
   * @return Matchers for templates creating direct internal links from parameter value.
   */
  public List<TemplateMatcher> getTemplateMatchers(String templateName) {
    if (templateMatchers != null) {
      return templateMatchers.get(normalizeTitle(templateName));
    }
    return null;
  }

  /**
   * @return Text indicating disambiguation repairing.
   */
  public String getDisambiguationString() {
    if (disambiguationText != null) {
      return disambiguationText;
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
    return formatComment(
           (((text != null) && (text.length() > 0)) ? text : "") +
           (((details != null) && (details.length() > 0)) ? " - " + details : ""));
  }

  /**
   * Format a comment.
   * 
   * @param comment Original comment.
   * @return Formatted comment (with WikiCleaner version).
   */
  public String formatComment(String comment) {
    Configuration config = Configuration.getConfiguration();
    boolean showWikiCleaner = config.getBoolean(
        Configuration.BOOLEAN_WIKICLEANER_COMMENT,
        Configuration.DEFAULT_WIKICLEANER_COMMENT);
    StringBuilder formattedComment = new StringBuilder();
    if (showWikiCleaner) {
      String link = getHelpPage();
      if ((link != null) && (link.trim().length() > 0)) {
        formattedComment.append("[[");
        formattedComment.append(link);
        formattedComment.append("|WPCleaner]] ");
      } else {
        formattedComment.append("WPCleaner");
      }
      formattedComment.append(Version.VERSION);
    }
    if (comment != null) {
      if (formattedComment.length() > 0) {
        formattedComment.append(" - ");
      }
      formattedComment.append(comment);
    }
    return formattedComment.toString();
  }

  /**
   * @return Wiktionary interwiki.
   */
  public String getWiktionaryInterwiki() {
    if (wiktionaryInterwiki != null) {
      return wiktionaryInterwiki;
    }
    return null;
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
  public List<Page> getTemplatesForHelpRequested() {
    if (templatesForHelpRequested != null) {
      List<Page> list = new ArrayList<Page>(templatesForHelpRequested.length);
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
   * @return Templates used for linking text.
   */
  public String[] getTemplatesForLinkingText() {
    if (templatesForLinkingText != null) {
      return templatesForLinkingText.clone();
    }
    return null;
  }

  /**
   * @return Pages containing the list of disambiguation pages.
   */
  public List<String> getDisambiguationList() {
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
        while ((posEnd == -1) && (!"END".equals(line))) {
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
    // Load configuration
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

    // Analyze configuration
    if (configuration != null) {
      String tmp;

      // Help URL
      tmp = configuration.getProperty("help_url", null);
      if ((tmp != null) && (tmp.trim().length() > 0)) {
        helpUrl = tmp.trim();
      }

      // Help Page
      tmp = configuration.getProperty("help_page", null);
      if ((tmp != null) && (tmp.trim().length() > 0)) {
        helpPage = tmp.trim();
      }

      // Pipe template
      tmp = configuration.getProperty("general_pipe_template", null);
      if ((tmp != null) && (tmp.trim().length() > 0)) {
        pipeTemplate = tmp.trim();
      }

      // Encyclopedic namespaces
      encyclopedicNamespaces = new ArrayList<Integer>();
      tmp = configuration.getProperty("general_encyclopedic_namespaces", null);
      if ((tmp != null) && (tmp.trim().length() > 0)) {
        List<String> tmpList = convertPropertyToStringList(tmp);
        if (tmpList != null) {
          for (String tmpValue : tmpList) {
            try {
              Integer value = Integer.valueOf(tmpValue);
              if (value.intValue() % 2 == 0) {
                encyclopedicNamespaces.add(value);
              }
            } catch (NumberFormatException e) {
              //
            }
          }
        }
      } else {
        encyclopedicNamespaces.add(Integer.valueOf(Namespace.MAIN));
        encyclopedicNamespaces.add(Integer.valueOf(Namespace.IMAGE));
        encyclopedicNamespaces.add(Integer.valueOf(Namespace.TEMPLATE));
        encyclopedicNamespaces.add(Integer.valueOf(Namespace.CATEGORY));
      }
      encyclopedicTalkNamespaces = new ArrayList<Integer>(encyclopedicNamespaces.size());
      for (Integer namespace : encyclopedicNamespaces) {
        encyclopedicTalkNamespaces.add(Integer.valueOf(namespace.intValue() + 1));
      }

      // Todo templates
      tmp = configuration.getProperty("general_todo_templates", null);
      if ((tmp != null) && (tmp.trim().length() > 0)) {
        todoTemplates = convertPropertyToStringList(tmp);
      }

      // Todo link templates
      tmp = configuration.getProperty("general_todo_link_templates", null);
      if ((tmp != null) && (tmp.trim().length() > 0)) {
        todoLinkTemplates = convertPropertyToStringList(tmp);
      }

      // Todo subpage
      tmp = configuration.getProperty("general_todo_subpage", null);
      if ((tmp != null) && (tmp.trim().length() > 0)) {
        todoSubpage = tmp.trim();
      }

      // Todo subpage force
      tmp = configuration.getProperty("general_todo_subpage_force", "false");
      if ((tmp != null) && (tmp.trim().length() > 0)) {
        todoSubpageForce = Boolean.parseBoolean(tmp.trim());
      }
      tmp = configuration.getProperty("general_todo_subpage_force_other", "false");
      if ((tmp != null) && (tmp.trim().length() > 0)) {
        todoSubpageForceOther = Boolean.parseBoolean(tmp.trim());
      }

      templateMatchers = new HashMap<String, List<TemplateMatcher>>();

      // Templates creating internal links from parameter value
      tmp = configuration.getProperty("general_dab_1l_templates", null);
      if ((tmp != null) && (tmp.trim().length() > 0)) {
        List<String> tmpList = convertPropertyToStringList(tmp);
        for (String template : tmpList) {
          String[] elements = template.split("\\|");
          String templateName = (elements.length > 0) ? normalizeTitle(elements[0]) : null;
          String parameterList = (elements.length > 1) ? elements[1] : null;
          String explanation = (elements.length > 2) ? elements[2] : null;
          String defaultValue = (elements.length > 3) ? elements[3] : null;
          String neededParameter = (elements.length > 4) ? elements[4] : null;
          if ((templateName != null) && (parameterList != null)) {
            List<TemplateMatcher> list = templateMatchers.get(templateName);
            if (list == null) {
              list = new ArrayList<TemplateMatcher>();
            }
            String[] parameterNames = parameterList.split(",");
            for (String parameterName : parameterNames) {
              TemplateMatcher matcher = new TemplateMatcher1L(
                  this, templateName, explanation, false, false,
                  parameterName, defaultValue, neededParameter);
              list.add(matcher);
            }
            templateMatchers.put(templateName, list);
          }
        }
      }
      tmp = configuration.getProperty("general_good_1l_templates", null);
      if ((tmp != null) && (tmp.trim().length() > 0)) {
        List<String> tmpList = convertPropertyToStringList(tmp);
        for (String template : tmpList) {
          String[] elements = template.split("\\|");
          String templateName = (elements.length > 0) ? normalizeTitle(elements[0]) : null;
          String parameterList = (elements.length > 1) ? elements[1] : null;
          String explanation = (elements.length > 2) ? elements[2] : null;
          String defaultValue = (elements.length > 3) ? elements[3] : null;
          String neededParameter = (elements.length > 4) ? elements[4] : null;
          if ((templateName != null) && (parameterList != null)) {
            List<TemplateMatcher> list = templateMatchers.get(templateName);
            if (list == null) {
              list = new ArrayList<TemplateMatcher>();
            }
            String[] parameterNames = parameterList.split(",");
            for (String parameterName : parameterNames) {
              TemplateMatcher matcher = new TemplateMatcher1L(
                  this, templateName, explanation, true, false,
                  parameterName, defaultValue, neededParameter);
              list.add(matcher);
            }
            templateMatchers.put(templateName, list);
          }
        }
      }
      tmp = configuration.getProperty("general_help_1l_templates", null);
      if ((tmp != null) && (tmp.trim().length() > 0)) {
        List<String> tmpList = convertPropertyToStringList(tmp);
        for (String template : tmpList) {
          String[] elements = template.split("\\|");
          String templateName = (elements.length > 0) ? normalizeTitle(elements[0]) : null;
          String parameterList = (elements.length > 1) ? elements[1] : null;
          String explanation = (elements.length > 2) ? elements[2] : null;
          String defaultValue = (elements.length > 3) ? elements[3] : null;
          String neededParameter = (elements.length > 4) ? elements[4] : null;
          if ((templateName != null) && (parameterList != null)) {
            List<TemplateMatcher> list = templateMatchers.get(templateName);
            if (list == null) {
              list = new ArrayList<TemplateMatcher>();
            }
            String[] parameterNames = parameterList.split(",");
            for (String parameterName : parameterNames) {
              TemplateMatcher matcher = new TemplateMatcher1L(
                  this, templateName, explanation, false, true,
                  parameterName, defaultValue, neededParameter);
              list.add(matcher);
            }
            templateMatchers.put(templateName, list);
          }
        }
      }

      // Templates creating internal links directly from parameter value
      tmp = configuration.getProperty("general_dab_1lt_templates", null);
      if ((tmp != null) && (tmp.trim().length() > 0)) {
        List<String> tmpList = convertPropertyToStringList(tmp);
        for (String template : tmpList) {
          String[] elements = template.split("\\|");
          String templateName = (elements.length > 0) ? normalizeTitle(elements[0]) : null;
          String parameterList = (elements.length > 1) ? elements[1] : null;
          String explanation = (elements.length > 2) ? elements[2] : null;
          String defaultValue = (elements.length > 3) ? elements[3] : null;
          String neededParameter = (elements.length > 4) ? elements[4] : null;
          if ((templateName != null) && (parameterList != null)) {
            List<TemplateMatcher> list = templateMatchers.get(templateName);
            if (list == null) {
              list = new ArrayList<TemplateMatcher>();
            }
            String[] parameterNames = parameterList.split(",");
            for (String parameterName : parameterNames) {
              TemplateMatcher matcher = new TemplateMatcher1LT(
                  this, templateName, explanation, false, false,
                  parameterName, defaultValue, neededParameter);
              list.add(matcher);
            }
            templateMatchers.put(templateName, list);
          }
        }
      }
      tmp = configuration.getProperty("general_good_1lt_templates", null);
      if ((tmp != null) && (tmp.trim().length() > 0)) {
        List<String> tmpList = convertPropertyToStringList(tmp);
        for (String template : tmpList) {
          String[] elements = template.split("\\|");
          String templateName = (elements.length > 0) ? normalizeTitle(elements[0]) : null;
          String parameterList = (elements.length > 1) ? elements[1] : null;
          String explanation = (elements.length > 2) ? elements[2] : null;
          String defaultValue = (elements.length > 3) ? elements[3] : null;
          String neededParameter = (elements.length > 4) ? elements[4] : null;
          if ((templateName != null) && (parameterList != null)) {
            List<TemplateMatcher> list = templateMatchers.get(templateName);
            if (list == null) {
              list = new ArrayList<TemplateMatcher>();
            }
            String[] parameterNames = parameterList.split(",");
            for (String parameterName : parameterNames) {
              TemplateMatcher matcher = new TemplateMatcher1LT(
                  this, templateName, explanation, true, false,
                  parameterName, defaultValue, neededParameter);
              list.add(matcher);
            }
            templateMatchers.put(templateName, list);
          }
        }
      }
      tmp = configuration.getProperty("general_help_1lt_templates", null);
      if ((tmp != null) && (tmp.trim().length() > 0)) {
        List<String> tmpList = convertPropertyToStringList(tmp);
        for (String template : tmpList) {
          String[] elements = template.split("\\|");
          String templateName = (elements.length > 0) ? normalizeTitle(elements[0]) : null;
          String parameterList = (elements.length > 1) ? elements[1] : null;
          String explanation = (elements.length > 2) ? elements[2] : null;
          String defaultValue = (elements.length > 3) ? elements[3] : null;
          String neededParameter = (elements.length > 4) ? elements[4] : null;
          if ((templateName != null) && (parameterList != null)) {
            List<TemplateMatcher> list = templateMatchers.get(templateName);
            if (list == null) {
              list = new ArrayList<TemplateMatcher>();
            }
            String[] parameterNames = parameterList.split(",");
            for (String parameterName : parameterNames) {
              TemplateMatcher matcher = new TemplateMatcher1LT(
                  this, templateName, explanation, false, true,
                  parameterName, defaultValue, neededParameter);
              list.add(matcher);
            }
            templateMatchers.put(templateName, list);
          }
        }
      }

      // Templates creating internal links from parameter value
      tmp = configuration.getProperty("general_dab_1l2t_templates", null);
      if ((tmp != null) && (tmp.trim().length() > 0)) {
        List<String> tmpList = convertPropertyToStringList(tmp);
        for (String template : tmpList) {
          String[] elements = template.split("\\|");
          String templateName = (elements.length > 0) ? normalizeTitle(elements[0]) : null;
          String parameterName1 = (elements.length > 1) ? elements[1] : null;
          String parameterName2 = (elements.length > 2) ? elements[2] : null;
          String explanation = (elements.length > 3) ? elements[3] : null;
          if ((templateName != null) && (parameterName1 != null) && (parameterName2 != null)) {
            List<TemplateMatcher> list = templateMatchers.get(templateName);
            if (list == null) {
              list = new ArrayList<TemplateMatcher>();
            }
            TemplateMatcher matcher = new TemplateMatcher1L2T(
                this, templateName, explanation,
                parameterName1, parameterName2);
            list.add(matcher);
            templateMatchers.put(templateName, list);
          }
        }
      }

      // Disambiguation comment
      tmp = configuration.getProperty("dab_comment", null);
      if ((tmp != null) && (tmp.trim().length() > 0)) {
        disambiguationText = tmp.trim();
      }

      // Disambiguation warning template
      tmp = configuration.getProperty("dab_warning_template", null);
      if ((tmp != null) && (tmp.trim().length() > 0)) {
        disambiguationWarningTemplate = tmp.trim();
      }

      // Disambiguation warning template comment
      tmp = configuration.getProperty("dab_warning_template_comment", null);
      if ((tmp != null) && (tmp.trim().length() > 0)) {
        disambiguationWarningTemplateComment = tmp.trim();
      }

      // Disambiguation warning comment
      tmp = configuration.getProperty("dab_warning_comment", null);
      if ((tmp != null) && (tmp.trim().length() > 0)) {
        disambiguationWarningComment = tmp.trim();
      }

      // Disambiguation warning comment done
      tmp = configuration.getProperty("dab_warning_comment_done", null);
      if ((tmp != null) && (tmp.trim().length() > 0)) {
        disambiguationWarningCommentDone = tmp.trim();
      }

      // Templates coming before disambiguation warning
      tmp = configuration.getProperty("dab_warning_after_templates", null);
      if ((tmp != null) && (tmp.trim().length() > 0)) {
        disambiguationWarningAfterTemplates = convertPropertyToStringList(tmp);
      }

      // Disambiguation list
      tmp = configuration.getProperty("dab_list", null);
      if ((tmp != null) && (tmp.trim().length() > 0)) {
        disambiguationList = convertPropertyToStringList(tmp);
      }

      // Templates for normal links to disambiguation pages
      tmp = configuration.getProperty("dab_link_templates", null);
      if ((tmp != null) && (tmp.trim().length() > 0)) {
        templatesForDisambiguationLink = convertPropertyToStringArray(tmp);
      }

      // Templates for needing help
      tmp = configuration.getProperty("needing_help_templates", null);
      if ((tmp != null) && (tmp.trim().length() > 0)) {
        templatesForNeedingHelp = convertPropertyToStringArray(tmp);
      }

      // Templates for help requested
      tmp = configuration.getProperty("help_requested_templates", null);
      if ((tmp != null) && (tmp.trim().length() > 0)) {
        templatesForHelpRequested = convertPropertyToStringArray(tmp);
      }

      // Templates for linking text
      tmp = configuration.getProperty("link_text_templates", null);
      if ((tmp != null) && (tmp.trim().length() > 0)) {
        templatesForLinkingText = convertPropertyToStringArray(tmp);
      }

      // Wiktionary interwiki
      tmp = configuration.getProperty("wikt_interwiki", null);
      if ((tmp != null) && (tmp.trim().length() > 0)) {
        wiktionaryInterwiki = tmp.trim();
      }

      // Wikitionary templates
      tmp = configuration.getProperty("wikt_templates", null);
      if ((tmp != null) && (tmp.trim().length() > 0)) {
        String[] results = convertPropertyToStringArray(tmp);
        if ((results != null) && (results.length > 0)) {
          wiktionaryMatches = new TemplateMatch[results.length];
          for (int i = 0; i < results.length; i++) {
            String[] elements = results[i].split("\\|");
            wiktionaryMatches[i] = new TemplateMatch(
                (elements.length > 0) ? elements[0].trim() : "",
                (elements.length > 1) ? elements[1].trim() : "",
                (elements.length > 2) ? elements[2].trim() : "",
                true, false);
          }
        }
      }

      // Check Wiki project page
      tmp = configuration.getProperty("check_wiki_project_page", null);
      if ((tmp != null) && (tmp.trim().length() > 0)) {
        checkWikiProject = tmp.trim();
      }

      // Check Wiki translation page
      tmp = configuration.getProperty("check_wiki_translation_page", null);
      if ((tmp != null) && (tmp.trim().length() > 0)) {
        checkWikiTranslation = tmp.trim();
      }
    }
  }

  /**
   * @param configuration Check Wiki configuration (general).
   */
  public void setCheckWikiGeneralConfiguration(Properties configuration) {
    this.checkWikiGeneralConfig = configuration;
  }

  private final DecimalFormat errorNumberFormat = new DecimalFormat("000");

  public String getCheckWikiProperty(
      String propertyName, int errorNumber,
      boolean useWiki, boolean useGeneral, boolean acceptEmpty) {
    String errorPrefix;
    synchronized (errorNumberFormat) {
      errorPrefix = "error_" + errorNumberFormat.format(errorNumber) + "_" + propertyName + "_";
    }
    String result = null;
    if ((useWiki) && (checkWikiConfig != null)) {
      result = checkWikiConfig.getProperty(errorPrefix + code + "wiki", null);
    }
    if ((result != null) && ((acceptEmpty) || (result.trim().length() > 0))) {
      return result.trim();
    }
    if ((useGeneral) && (checkWikiGeneralConfig != null)) {
      result = checkWikiGeneralConfig.getProperty(errorPrefix + "script", null);
    }
    if (result != null) {
      return result.trim();
    }
    return null;
  }

  /**
   * @param configuration Check Wiki configuration for this Wiki.
   */
  public void setCheckWikiConfiguration(Properties configuration) {
    this.checkWikiConfig = configuration;
  }

  /**
   * Convert a multi-line property to a string array.
   * 
   * @param property Property.
   * @return String array.
   */
  public String[] convertPropertyToStringArray(String property) {
    String[] result = null;
    if ((property != null) && (property.trim().length() > 0)) {
      String[] results = property.trim().split("\n");
      if ((results != null) && (results.length > 0)) {
        int count = 0;
        for (int i = 0; i < results.length; i++) {
          results[i] = results[i].trim();
          if (results[i].length() > 0) {
            count++;
          }
        }
        result = new String[count];
        for (int i = results.length; i > 0; i--) {
          if (results[i - 1].length() > 0) {
            count--;
            result[count] = results[i - 1];
          }
        }
      }
    }
    return result;
  }

  /**
   * Convert a multi-line property to a string list.
   * 
   * @param property Property.
   * @return String list.
   */
  public List<String> convertPropertyToStringList(String property) {
    List<String> result = null;
    if ((property != null) && (property.trim().length() > 0)) {
      String[] results = property.trim().split("\n");
      if ((results != null) && (results.length > 0)) {
        result = new ArrayList<String>();
        for (int  i = 0; i < results.length; i++) {
          results[i] = results[i].trim();
          if (results[i].length() > 0) {
            result.add(results[i]);
          }
        }
      }
    }
    return result;
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
          api.retrieveLinks(this, page, null);
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
   * @return List of disambiguation templates 
   */
  public List<Page> getDisambiguationTemplates() {
    return disambiguationTemplates;
  }

  /**
   * @return Flag indicating if the Check Wiki project is available.
   */
  public boolean isCheckWikiProjectAvailable() {
    String project = getCheckWikiProject();
    if (project != null) {
      return true;
    }
    if (configuration != null) {
      String tmp = configuration.getProperty("check_wiki_force", null);
      if ((tmp != null) && Boolean.valueOf(tmp)) {
        return true;
      }
    }
    return false;
  }

  /**
   * @return Check Wikipedia Project page.
   */
  public String getCheckWikiProject() {
    if (checkWikiProject != null) {
      return checkWikiProject;
    }
    return null;
  }

  /**
   * @return Check Wikipedia Project traduction.
   */
  public String getCheckWikiTraduction() {
    if (checkWikiTranslation != null) {
      return checkWikiTranslation;
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
        (getMagicWord(MagicWord.IMG_UPRIGHT).isPossibleAlias(text, "[0-9 ]*")) ||
        (getMagicWord(MagicWord.IMG_WIDTH).isPossibleAlias(text, "[0-9 ]*"))) {
      return true;
    }
    return false;
  }

  /**
   * @param magicWords Magic words.
   */
  public void setMagicWords(Map<String, MagicWord> magicWords) {
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
