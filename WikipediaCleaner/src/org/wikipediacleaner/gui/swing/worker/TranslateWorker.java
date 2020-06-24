/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.worker;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementCategory;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueBoolean;


/**
 * SwingWorker for translating a page.
 */
public class TranslateWorker extends BasicWorker {

  private final EnumWikipedia from;
  private final Page page;
  private final String initialText;

  /**
   * @param wikipedia Wikipedia.
   * @param window Window.
   * @param from Original Wikipedia.
   * @param page Page.
   * @param text Page contents.
   */
  public TranslateWorker(
      EnumWikipedia wikipedia, BasicWindow window,
      EnumWikipedia from,
      Page page, String text) {
    super(wikipedia, window);
    this.from = from;
    this.page = page;
    this.initialText = text;
  }

  /**
   * @return Translated text.
   * @see org.wikipediacleaner.gui.swing.basic.BasicWorker#construct()
   */
  @Override
  public Object construct() {
    String text = initialText;

    try {
      Configuration config = Configuration.getConfiguration();
      text = translateInternalLinks(
          text,
          config.getBoolean(null, ConfigurationValueBoolean.TRANSLATION_INTERNAL_LINK_TEXT),
          config.getBoolean(null, ConfigurationValueBoolean.TRANSLATION_INTERLANGUAGE));
      text = translateCategories(
          text,
          config.getBoolean(null, ConfigurationValueBoolean.TRANSLATION_CATEGORY));
      text = translateTemplates(
          text,
          config.getBoolean(null, ConfigurationValueBoolean.TRANSLATION_TEMPLATE_NAME),
          config.getBoolean(null, ConfigurationValueBoolean.TRANSLATION_TEMPLATE_NO_PARAM));
    } catch (APIException e) {
      return null;
    }

    return text;
  }

  /**
   * @param text Text to translate.
   * @param translateText Flag indicating if internal link text should be translated.
   * @param useInterLanguage Flag indicating if interlanguage links can be used.
   * @return Text with internal links translated.
   * @throws APIException
   */
  private String translateInternalLinks(
      String text,
      boolean translateText,
      boolean useInterLanguage) throws APIException {
    PageAnalysis analysis = page.getAnalysis(text, true);
    Collection<PageElementInternalLink> links = analysis.getInternalLinks();
    Map<String, String> interwikis = new HashMap<String, String>();
    StringBuilder newText = new StringBuilder();
    int lastPosition = 0;
    for (PageElementInternalLink link : links) {
      String linkPage = link.getLink();
      setText(GT._T("Retrieving interwiki for {0}", linkPage));
      String translated = null;
      if (!interwikis.containsKey(linkPage)) {
        translated = getLanguageLink(linkPage);
        interwikis.put(linkPage, translated);
      } else {
        translated = interwikis.get(linkPage);
      }
      if (translated != null) {
        if (!Page.areSameTitle(linkPage, translated)) {
          if (link.getBeginIndex() > lastPosition) {
            newText.append(text.substring(lastPosition, link.getBeginIndex()));
            lastPosition = link.getBeginIndex();
          }
          newText.append("[[");
          if (translateText && (link.getText() == null)) {
            String displayed = link.getDisplayedText();
            if ((displayed != null) &&
                (displayed.length() > 0) &&
                (Character.isLowerCase(displayed.charAt(0)))) {
              if (translated.length() > 1) {
                translated = translated.substring(0, 1).toLowerCase() + translated.substring(1);
              } else {
                translated = translated.toLowerCase();
              }
            }
          }
          newText.append(translated);
          if ((translated.indexOf('#') < 0) && (link.getAnchor() != null)) {
            newText.append("#");
            newText.append(link.getAnchor());
          }
          if (!translateText || (link.getText() != null)) {
            newText.append("|");
            newText.append(link.getDisplayedText());
          }
          newText.append("]]");
          lastPosition = link.getEndIndex();
        }
      } else {
        if (useInterLanguage) {
          if (link.getBeginIndex() > lastPosition) {
            newText.append(text.substring(lastPosition, link.getBeginIndex()));
            lastPosition = link.getEndIndex();
          }
          newText.append("[[:");
          newText.append(from.getSettings().getLanguage());
          newText.append(":");
          newText.append(link.getFullLink());
          newText.append("|");
          newText.append(link.getDisplayedText());
          newText.append("]]");
          lastPosition = link.getEndIndex();
        }
      }
    }
    if (newText.length() == 0) {
      return text;
    }
    if (lastPosition < text.length()) {
      newText.append(text.substring(lastPosition));
      lastPosition = text.length();
    }
    return newText.toString();
  }

  /**
   * @param text Text to translate.
   * @param translate Flag indicating if categories should be translated.
   * @return Text with categories translated.
   * @throws APIException
   */
  private String translateCategories(
      String text, boolean translate) throws APIException {
    if (!translate) {
      return text;
    }
    Namespace categoryNamespace = getWikipedia().getWikiConfiguration().getNamespace(Namespace.CATEGORY);
    if (categoryNamespace == null) {
      return text;
    }
    PageAnalysis analysis = page.getAnalysis(text, true);
    Collection<PageElementCategory> categories = analysis.getCategories();
    Map<String, String> interwikis = new HashMap<String, String>();
    StringBuilder newText = new StringBuilder();
    int lastPosition = 0;
    for (PageElementCategory category : categories) {
      String categoryName = category.getName();
      String fullCategoryName = categoryNamespace.getCanonicalTitle() + ":" + categoryName;
      setText(GT._T("Retrieving interwiki for {0}", fullCategoryName));
      String translated = null;
      if (!interwikis.containsKey(categoryName)) {
        translated = getLanguageLink(fullCategoryName);
        interwikis.put(categoryName, translated);
      } else {
        translated = interwikis.get(categoryName);
      }
      if ((translated != null) && !Page.areSameTitle(categoryName, translated)) {
        if (category.getBeginIndex() > lastPosition) {
          newText.append(text.substring(lastPosition, category.getBeginIndex()));
          lastPosition = category.getBeginIndex();
        }
        newText.append("[[");
        newText.append(translated);
        if (category.getSort() != null) {
          newText.append("|");
          newText.append(category.getSort());
        }
        newText.append("]]");
        lastPosition = category.getEndIndex();
      }
    }
    if (newText.length() == 0) {
      return text;
    }
    if (lastPosition < text.length()) {
      newText.append(text.substring(lastPosition));
      lastPosition = text.length();
    }
    return newText.toString();
  }

  /**
   * @param text Text to translate.
   * @param translateName Flag indicating if templates names should be translated.
   * @param translateWithoutParams Flag indicating if templates without parameters should be translated.
   * @return Text with templates translated.
   * @throws APIException
   */
  private String translateTemplates(
      String text,
      boolean translateName,
      boolean translateWithoutParams) throws APIException {
    if (!translateName) {
      return text;
    }
    Namespace templateNamespace = getWikipedia().getWikiConfiguration().getNamespace(Namespace.TEMPLATE);
    if (templateNamespace == null) {
      return text;
    }
    PageAnalysis analysis = page.getAnalysis(text, true);
    Collection<PageElementTemplate> templates = analysis.getTemplates();
    Map<String, String> interwikis = new HashMap<String, String>();
    StringBuilder newText = new StringBuilder();
    int lastPosition = 0;
    for (PageElementTemplate template : templates) {
      String templateName = template.getTemplateName();
      String fullTemplateName = templateNamespace.getCanonicalTitle() + ":" + templateName;
      setText(GT._T("Retrieving interwiki for {0}", fullTemplateName));
      String translated = null;
      if (!interwikis.containsKey(templateName)) {
        translated = getLanguageLink(fullTemplateName);
        interwikis.put(templateName, translated);
      } else {
        translated = interwikis.get(templateName);
      }
      if ((translated != null) && !Page.areSameTitle(templateName, translated)) {
        if (template.getBeginIndex() > lastPosition) {
          newText.append(text.substring(lastPosition, template.getBeginIndex()));
          lastPosition = template.getBeginIndex();
        }
        if (translateWithoutParams && (template.getParameterCount() == 0)) {
          newText.append("{{");
          int columnPos = translated.indexOf(':');
          if (columnPos < 0) {
            newText.append(translated);
          } else {
            newText.append(translated.substring(columnPos + 1));
          }
          newText.append("}}");
          lastPosition = template.getEndIndex();
        } else {
          newText.append("<!-- ");
          newText.append(translated);
          newText.append(" -->");
        }
      }
    }
    if (newText.length() == 0) {
      return text;
    }
    if (lastPosition < text.length()) {
      newText.append(text.substring(lastPosition));
      lastPosition = text.length();
    }
    return newText.toString();
  }

  /**
   * @param pageName Page name.
   * @return Language link.
   * @throws APIException
   */
  private String getLanguageLink(String pageName) throws APIException {
    API api = APIFactory.getAPI();
    String link = api.getLanguageLink(from, getWikipedia(), pageName);
    if (link != null) {
      return link;
    }
    Page original = DataManager.getPage(from, pageName, null, null, null);
    //api.retrieveLinksWithRedirects(from, original, null, null);
    api.initializeRedirect(from, Collections.singletonList(original));
    if (!original.getRedirects().isRedirect()) {
      return link;
    }
    api.retrieveContents(from, Collections.singletonList(original), false, true);
    link = api.getLanguageLink(from, getWikipedia(), original.getRedirects().getTitle());
    if (link == null) {
      return null;
    }
    String destination = original.getRedirects().getDestination();
    int anchorPos = destination.indexOf('#');
    if (anchorPos < 0) {
      return link;
    }
    return link + destination.substring(anchorPos);
  }
}
