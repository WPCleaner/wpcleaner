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
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;

import org.wikipediacleaner.Version;
import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.wiki.AbstractWikiSettings;
import org.wikipediacleaner.api.constants.wiki.WikipediaAf;
import org.wikipediacleaner.api.constants.wiki.WikipediaAls;
import org.wikipediacleaner.api.constants.wiki.WikipediaAr;
import org.wikipediacleaner.api.constants.wiki.WikipediaBar;
import org.wikipediacleaner.api.constants.wiki.WikipediaCa;
import org.wikipediacleaner.api.constants.wiki.WikipediaCommons;
import org.wikipediacleaner.api.constants.wiki.WikipediaCs;
import org.wikipediacleaner.api.constants.wiki.WikipediaCy;
import org.wikipediacleaner.api.constants.wiki.WikipediaDa;
import org.wikipediacleaner.api.constants.wiki.WikipediaDe;
import org.wikipediacleaner.api.constants.wiki.WikipediaEl;
import org.wikipediacleaner.api.constants.wiki.WikipediaEn;
import org.wikipediacleaner.api.constants.wiki.WikipediaEo;
import org.wikipediacleaner.api.constants.wiki.WikipediaEs;
import org.wikipediacleaner.api.constants.wiki.WikipediaFi;
import org.wikipediacleaner.api.constants.wiki.WikipediaFr;
import org.wikipediacleaner.api.constants.wiki.WikipediaFy;
import org.wikipediacleaner.api.constants.wiki.WikipediaGd;
import org.wikipediacleaner.api.constants.wiki.WikipediaHe;
import org.wikipediacleaner.api.constants.wiki.WikipediaHif;
import org.wikipediacleaner.api.constants.wiki.WikipediaHu;
import org.wikipediacleaner.api.constants.wiki.WikipediaId;
import org.wikipediacleaner.api.constants.wiki.WikipediaIs;
import org.wikipediacleaner.api.constants.wiki.WikipediaIt;
import org.wikipediacleaner.api.constants.wiki.WikipediaJa;
import org.wikipediacleaner.api.constants.wiki.WikipediaKo;
import org.wikipediacleaner.api.constants.wiki.WikipediaLa;
import org.wikipediacleaner.api.constants.wiki.WikipediaNds;
import org.wikipediacleaner.api.constants.wiki.WikipediaNdsNl;
import org.wikipediacleaner.api.constants.wiki.WikipediaNl;
import org.wikipediacleaner.api.constants.wiki.WikipediaNo;
import org.wikipediacleaner.api.constants.wiki.WikipediaPdc;
import org.wikipediacleaner.api.constants.wiki.WikipediaPl;
import org.wikipediacleaner.api.constants.wiki.WikipediaPt;
import org.wikipediacleaner.api.constants.wiki.WikipediaRo;
import org.wikipediacleaner.api.constants.wiki.WikipediaRu;
import org.wikipediacleaner.api.constants.wiki.WikipediaSimple;
import org.wikipediacleaner.api.constants.wiki.WikipediaSk;
import org.wikipediacleaner.api.constants.wiki.WikipediaSl;
import org.wikipediacleaner.api.constants.wiki.WikipediaSv;
import org.wikipediacleaner.api.constants.wiki.WikipediaTr;
import org.wikipediacleaner.api.constants.wiki.WikipediaUk;
import org.wikipediacleaner.api.constants.wiki.WikipediaVi;
import org.wikipediacleaner.api.constants.wiki.WikipediaYi;
import org.wikipediacleaner.api.constants.wiki.WikipediaZh;
import org.wikipediacleaner.api.constants.wiki.WikisourceFr;
import org.wikipediacleaner.api.constants.wiki.WikiversityFr;
import org.wikipediacleaner.api.constants.wiki.WiktionaryEn;
import org.wikipediacleaner.api.constants.wiki.WiktionaryFr;
import org.wikipediacleaner.api.constants.wiki.WiktionarySv;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueBoolean;


/**
 * Encapsulate possible Wikipedia sites.
 */
public enum EnumWikipedia {

  /*
   * List of Wikipedia WPCleaner is able to deal with.
   * 
   * For each Wikipedia, a configuration object is required.
   */

  AF(new WikipediaAf()),
  ALS(new WikipediaAls()),
  AR(new WikipediaAr()),
  BAR(new WikipediaBar()),
  CA(new WikipediaCa()),
  CS(new WikipediaCs()),
  CY(new WikipediaCy()),
  DA(new WikipediaDa()),
  DE(new WikipediaDe()),
  EL(new WikipediaEl()),
  EN(new WikipediaEn()),
  EO(new WikipediaEo()),
  ES(new WikipediaEs()),
  FI(new WikipediaFi()),
  FR(new WikipediaFr()),
  FY(new WikipediaFy()),
  GD(new WikipediaGd()),
  HE(new WikipediaHe()),
  HIF(new WikipediaHif()),
  HU(new WikipediaHu()),
  ID(new WikipediaId()),
  IS(new WikipediaIs()),
  IT(new WikipediaIt()),
  JA(new WikipediaJa()),
  Ko(new WikipediaKo()),
  LA(new WikipediaLa()),
  NDS(new WikipediaNds()),
  NDS_NL(new WikipediaNdsNl()),
  NL(new WikipediaNl()),
  NO(new WikipediaNo()),
  PDC(new WikipediaPdc()),
  PL(new WikipediaPl()),
  PT(new WikipediaPt()),
  RO(new WikipediaRo()),
  RU(new WikipediaRu()),
  SIMPLE(new WikipediaSimple()),
  SK(new WikipediaSk()),
  SL(new WikipediaSl()),
  SV(new WikipediaSv()),
  TR(new WikipediaTr()),
  UK(new WikipediaUk()),
  VI(new WikipediaVi()),
  YI(new WikipediaYi()),
  ZH(new WikipediaZh()),
  COMMONS(new WikipediaCommons()),

  WIKTIONARY_EN(new WiktionaryEn()),
  WIKTIONARY_FR(new WiktionaryFr()),
  WIKTIONARY_SV(new WiktionarySv()),

  WIKISOURCE_FR(new WikisourceFr()),

  WIKIVERSITY_FR(new WikiversityFr());

  private final AbstractWikiSettings settings;

  private final String configPage;
  private Set<String> disambiguationPages;
  private List<Page> disambiguationTemplates;

  /**
   * Wiki configuration.
   */
  private final WikiConfiguration wikiConfiguration;

  /**
   * WPCleaner configuration.
   */
  private final WPCConfiguration WPCConfiguration;

  /**
   * Configuration for Check Wiki project.
   */
  private final CWConfiguration CWConfiguration;

  /**
   * @param settigs Wiki settings.
   */
  EnumWikipedia(AbstractWikiSettings settings) {
    this.settings = settings;
    this.configPage = settings.getConfigurationPage();
    this.wikiConfiguration = new WikiConfiguration();
    this.WPCConfiguration = new WPCConfiguration(this);
    this.CWConfiguration = new CWConfiguration(settings.getCodeCheckWiki());
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
      if (e.getSettings().getCode().equals(code)) {
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
        if (e.getSettings().getCode().equals(locale.getLanguage())) {
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
   * @return Wiki settings.
   */
  public AbstractWikiSettings getSettings() {
    return settings;
  }

  /**
   * @return Wiki configuration.
   */
  public WikiConfiguration getWikiConfiguration() {
    return wikiConfiguration;
  }

  /**
   * @return Configuration page.
   */
  public String getConfigurationPage() {
    return getUserConfigurationPage("NicoV");
  }

  /**
   * @param userName User name.
   * @return Configuration page.
   */
  public String getUserConfigurationPage(String userName) {
    Namespace userNamespace = wikiConfiguration.getNamespace(Namespace.USER);
    String userPrefix = (userNamespace != null) ? userNamespace.getTitle() : "User";
    return userPrefix + ":" + userName + "/" + configPage;
  }

  /* ========================================================================= */
  /* User management                                                           */
  /* ========================================================================= */

  /**
   * Connection information.
   */
  private final ConnectionInformation connection = new ConnectionInformation();

  /**
   * Contributions.
   */
  private final Contributions contributions = new Contributions(this);

  /**
   * @return Connection information.
   */
  public ConnectionInformation getConnection() {
    return connection;
  }

  /**
   * @return Contributions
   */
  public Contributions getContributions() {
    return contributions;
  }

  /**
   * @param text Comment.
   * @param details Details about the update.
   * @param automatic True if this an automatic edit.
   * @return Full comment.
   */
  public String createUpdatePageComment(String text, String details, boolean automatic) {
    return formatComment(
           (((text != null) && (text.length() > 0)) ? text : "") +
           (((details != null) && (details.length() > 0)) ? " - " + details : ""),
           automatic);
  }

  /**
   * Format a comment.
   * 
   * @param comment Original comment.
   * @param automatic True if this an automatic edit.
   * @return Formatted comment (with WPCleaner version).
   */
  public String formatComment(String comment, boolean automatic) {
    Configuration config = Configuration.getConfiguration();
    boolean showWikiCleaner = config.getBoolean(
        null,
        ConfigurationValueBoolean.WIKICLEANER_COMMENT);
    StringBuilder formattedComment = new StringBuilder();
    if (showWikiCleaner) {
      String link = WPCConfiguration.getString(WPCConfigurationString.HELP_PAGE);
      if ((link != null) && (link.trim().length() > 0)) {
        formattedComment.append("[[");
        formattedComment.append(link);
        formattedComment.append("|");
        formattedComment.append(Version.PROGRAM);
        formattedComment.append("]] ");
      } else {
        formattedComment.append(Version.PROGRAM);
        formattedComment.append(" ");
      }
      formattedComment.append("v");
      formattedComment.append(Version.VERSION);
      if (automatic) {
        formattedComment.append("b");
      }
    }
    if (comment != null) {
      if (formattedComment.length() > 0) {
        formattedComment.append(" - ");
      }
      formattedComment.append(comment);
    }
    if (!showWikiCleaner) {
      if (formattedComment.length() > 0) {
        formattedComment.append(" ");
      }
      formattedComment.append("(v");
      formattedComment.append(Version.VERSION);
      if (automatic) {
        formattedComment.append("b");
      }
      formattedComment.append(")");
    }
    return formattedComment.toString();
  }

  /**
   * Construct list of disambiguation pages.
   * 
   * @param api Wikipedia API
   * @return List of disambiguation pages
   * @throws APIException
   */
  public List<Page> constuctDisambiguationPages(API api) throws APIException {
    ArrayList<Page> tmpResult = new ArrayList<Page>();
    List<Page> dabCategories = getConfiguration().getDisambiguationCategories();
    if ((dabCategories != null) && (dabCategories.size() > 0)) {
      for (Page dabCategory : dabCategories) {
        api.retrieveCategoryMembers(
            this, dabCategory, 0, false);
        List<Page> tmpPages = dabCategory.getRelatedPages(Page.RelatedPages.CATEGORY_MEMBERS);
        if (tmpPages != null) {
          tmpResult.ensureCapacity(tmpResult.size() + tmpPages.size());
          for (Page page : tmpPages) {
            if (page.isInMainNamespace()) {
              tmpResult.add(page);
            }
          }
        }
      }
    } else {
      if (disambiguationTemplates == null) {
        return null;
      }
      for (Page dabTemplate : disambiguationTemplates) {
        api.retrieveEmbeddedIn(
            this, dabTemplate,
            Collections.singletonList(Namespace.MAIN),
            false);
        List<Page> tmpPages = dabTemplate.getRelatedPages(Page.RelatedPages.EMBEDDED_IN);
        if (tmpPages != null) {
          tmpResult.ensureCapacity(tmpResult.size() + tmpPages.size());
          for (Page page : tmpPages) {
            if (page.isInMainNamespace()) {
              tmpResult.add(page);
            }
          }
        }
      }
    }
    return tmpResult;
  }

  /**
   * Load all disambiguation pages.
   * 
   * @param api Wikipedia API.
   * @throws APIException
   */
  public void loadDisambiguationPages(API api) throws APIException {
    try {
      List<Page> tmpPages = constuctDisambiguationPages(api);
      HashSet<String> tmpResult = new HashSet<String>();
      for (Page page : tmpPages) {
        tmpResult.add(page.getTitle());
      }
      disambiguationPages = tmpResult;
    } catch (APIException e) {
      disambiguationPages = null;
      throw e;
    }
  }

  /**
   * @param dabPages List of disambiguation pages.
   */
  public void setDisambiguationPages(Set<String> dabPages) {
    if (dabPages == null) {
      return;
    }
    HashSet<String> tmpResult = new HashSet<String>(dabPages.size());
    tmpResult.addAll(dabPages);
    disambiguationPages = tmpResult;
  }

  /**
   * @return true if disambiguation pages have been loaded.
   */
  public boolean isDisambiguationPagesLoaded() {
    return (disambiguationPages != null);
  }

  /**
   * Tell if a page is a disambiguation page.
   * 
   * @param page Page
   * @return TRUE if it's a disambiguation page.
   */
  public Boolean isDisambiguationPage(Page page) {
    if (page == null) {
      return null;
    }
    if (disambiguationPages == null) {
      return null;
    }
    if (disambiguationPages.contains(page.getTitle())) {
      return Boolean.TRUE;
    }
    if (page.isRedirect()) {
      for (Page redirect : page.getRedirects()) {
        if ((redirect != null) &&
            (disambiguationPages.contains(redirect.getTitle()))) {
          return Boolean.TRUE;
        }
      }
    }
    return Boolean.FALSE;
  }

  /**
   * Load all disambiguation templates.
   * 
   * @param api Wikipedia API.
   */
  public void initDisambiguationTemplates(API api) {
    if (disambiguationTemplates == null) {
      synchronized (api) {
        Page page = DataManager.getPage(
            this, "Mediawiki:Disambiguationspage",
            null, null, null);
        try {
          api.retrieveLinks(this, Collections.singletonList(page));
        } catch (APIException e) {
          // Error retrieving Disambiguation templates list
        }
        disambiguationTemplates = page.getLinks();
      }
    }
  }
  
  /**
   * @param pageName Page name.
   * @param api Wikipedia API.
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
   * @return WPCleaner configuration.
   */
  public WPCConfiguration getConfiguration() {
    return WPCConfiguration;
  }

  /**
   * @return Check Wiki project configuration.
   */
  public CWConfiguration getCWConfiguration() {
    return CWConfiguration;
  }

  /* (non-Javadoc)
   * @see java.lang.Enum#toString()
   */
  @Override
  public String toString() {
    return settings.toString();
  }
}
