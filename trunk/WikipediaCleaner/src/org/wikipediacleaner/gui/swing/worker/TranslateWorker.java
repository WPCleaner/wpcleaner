/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2012  Nicolas Vervelle
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

package org.wikipediacleaner.gui.swing.worker;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.wikipediacleaner.api.base.API;
import org.wikipediacleaner.api.base.APIException;
import org.wikipediacleaner.api.base.APIFactory;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementCategory;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.i18n.GT;


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
      text = translateInternalLinks(text);
      text = translateCategories(text);
      text = translateTemplates(text);
    } catch (APIException e) {
      return null;
    }

    return text;
  }

  /**
   * @param text Text to translate.
   * @return Text with internal links translated.
   * @throws APIException
   */
  private String translateInternalLinks(String text) throws APIException {
    API api = APIFactory.getAPI();
    PageAnalysis analysis = new PageAnalysis(page, text);
    Collection<PageElementInternalLink> links = analysis.getInternalLinks();
    Map<String, String> interwikis = new HashMap<String, String>();
    StringBuilder newText = new StringBuilder();
    int lastPosition = 0;
    for (PageElementInternalLink link : links) {
      String linkPage = link.getLink();
      setText(GT._("Retrieving interwiki for {0}", linkPage));
      String translated = null;
      if (!interwikis.containsKey(linkPage)) {
        translated = api.getLanguageLink(from, getWikipedia(), linkPage);
        interwikis.put(linkPage, translated);
      } else {
        translated = interwikis.get(linkPage);
      }
      if ((translated != null) && !Page.areSameTitle(linkPage, translated)) {
        if (link.getBeginIndex() > lastPosition) {
          newText.append(text.substring(lastPosition, link.getBeginIndex()));
          lastPosition = link.getBeginIndex();
        }
        newText.append("[[");
        newText.append(translated);
        newText.append("|");
        newText.append(link.getDisplayedText());
        newText.append("]]");
        lastPosition = link.getEndIndex();
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
   * @return Text with categories translated.
   * @throws APIException
   */
  private String translateCategories(String text) throws APIException {
    Namespace categoryNamespace = Namespace.getNamespace(
        Namespace.CATEGORY, getWikipedia().getNamespaces());
    if (categoryNamespace == null) {
      return text;
    }
    API api = APIFactory.getAPI();
    PageAnalysis analysis = new PageAnalysis(page, text);
    Collection<PageElementCategory> categories = analysis.getCategories();
    Map<String, String> interwikis = new HashMap<String, String>();
    StringBuilder newText = new StringBuilder();
    int lastPosition = 0;
    for (PageElementCategory category : categories) {
      String categoryName = category.getName();
      String fullCategoryName = categoryNamespace.getCanonicalTitle() + ":" + categoryName;
      setText(GT._("Retrieving interwiki for {0}", fullCategoryName));
      String translated = null;
      if (!interwikis.containsKey(categoryName)) {
        translated = api.getLanguageLink(from, getWikipedia(), fullCategoryName);
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
   * @return Text with templates translated.
   * @throws APIException
   */
  private String translateTemplates(String text) throws APIException {
    API api = APIFactory.getAPI();
    PageAnalysis analysis = new PageAnalysis(page, text);
    Collection<PageElementTemplate> templates = analysis.getTemplates();
    Map<String, String> interwikis = new HashMap<String, String>();
    StringBuilder newText = new StringBuilder();
    int lastPosition = 0;
    for (PageElementTemplate template : templates) {
      String templateName = template.getTemplateName();
      String fullTemplateName = Namespace.getTitle(
          Namespace.TEMPLATE, getWikipedia().getNamespaces(), templateName);
      setText(GT._("Retrieving interwiki for {0}", fullTemplateName));
      String translated = null;
      if (!interwikis.containsKey(templateName)) {
        translated = api.getLanguageLink(from, getWikipedia(), fullTemplateName);
        interwikis.put(templateName, translated);
      } else {
        translated = interwikis.get(templateName);
      }
      if ((translated != null) && !Page.areSameTitle(templateName, translated)) {
        if (template.getBeginIndex() > lastPosition) {
          newText.append(text.substring(lastPosition, template.getBeginIndex()));
          lastPosition = template.getBeginIndex();
        }
        newText.append("<!-- ");
        newText.append(translated);
        newText.append(" -->");
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
}
