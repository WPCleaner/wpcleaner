/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2022  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm.a0xx.a09x.a090;

import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.constants.ArticleUrl;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementExternalLink;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;

/**
 * Bean for holding analysis information.
 */
class AnalysisInformation {

  /** Page analysis */
  public final PageAnalysis analysis;

  /** Article contents */
  public final String contents;

  /** External link */
  public final PageElementExternalLink link;

  /** Information about article URL */
  public final ArticleUrl articleUrl;

  /** Article title */
  public final String article;

  /** Wiki */
  public final EnumWikipedia wiki;

  /** Errors */
  public final Collection<CheckErrorResult> errors;

  /** Begin index */
  public int beginIndex;

  /** End index */
  public int endIndex;

  /** Link text */
  public String text;

  /** True if replacement can be automatic */
  public Boolean automatic;

  /** True if internal needs to be prefixed with a colon */
  public Boolean needColon;

  /** True if link is defined by a template */
  public Boolean isInTemplate;

  public AnalysisInformation(
      PageAnalysis analysis,
      PageElementExternalLink link,
      ArticleUrl articleUrl,
      EnumWikipedia wiki,
      Collection<CheckErrorResult> errors) {
    this.analysis = analysis;
    this.contents = analysis.getContents();
    this.link = link;
    this.articleUrl = articleUrl;
    this.article = articleUrl.getTitle();
    this.wiki = wiki;
    this.errors = errors;
    this.beginIndex = link.getBeginIndex();
    this.endIndex = link.getEndIndex();
    String tmpText = link.getText();
    if (tmpText != null) {
      while (tmpText.startsWith("|") || tmpText.startsWith(" ")) {
        tmpText = tmpText.substring(1);
      }
    }
    this.text = tmpText;
    this.automatic = Boolean.TRUE;
    this.needColon = null;
    this.isInTemplate = null;
  }

  /**
   * Compute complementary information about the link
   */
  public void computeLinkInformation() {
    if (link.hasSquare()) {
      if ((beginIndex > 0) && (contents.charAt(beginIndex - 1) == '[') &&
          (endIndex < contents.length()) && (contents.charAt(endIndex) == ']')) {
        beginIndex--;
        endIndex++;
      }
    }
    Page articlePage = DataManager.createSimplePage(
        analysis.getWikipedia(), article, null, null, null);
    needColon = Boolean.FALSE;
    if (articlePage.getNamespace() != null) {
      int ns = articlePage.getNamespace().intValue();
      if (ns % 2 == 0) {
        if ((ns != Namespace.MAIN) &&
            (ns != Namespace.USER) &&
            (ns != Namespace.HELP) &&
            (ns != Namespace.MEDIAWIKI) &&
            (ns != Namespace.TEMPLATE) &&
            (ns != Namespace.WIKIPEDIA)) {
          needColon = Boolean.TRUE;
        }
      }
    }
  }

  /**
   * Compute if link is defined by a template.
   * 
   * @param linkTemplates List of link templates.
   */
  public void computeIsInTemplate(List<String[]> linkTemplates) {
    
    isInTemplate = Boolean.FALSE;
    if (linkTemplates != null) {
      PageElementTemplate template = analysis.isInTemplate(beginIndex);
      if (template != null) {
        for (String[] elements : linkTemplates) {
          if ((elements.length > 2) &&
              Page.areSameTitle(elements[0], template.getTemplateName()) &&
              link.getLink().trim().equals(template.getParameterValue(elements[1]))) {
            text = template.getParameterValue(elements[2]);
            beginIndex = template.getBeginIndex();
            endIndex = template.getEndIndex();
            isInTemplate = true;
          }
        }
      }
    }
  }

  /**
   * Compute restrictions on automatic replacement.
   */
  public void computeIsAutomatic() {
    if (Page.areSameTitle(article, analysis.getPage().getTitle())) {
      automatic = false;
    }
    if (articleUrl.getAttributes() != null) {
      for (Map.Entry<String, String> attribute : articleUrl.getAttributes().entrySet()) {
        String key = attribute.getKey();
        if ("venotify".equals(key)) {
          if (!"created".equals(attribute.getValue())) {
            automatic = false;
          }
        } else if ("action".equals(key)) {
          //if (!"edit".equals(attribute.getValue())) {
            automatic = false;
          //}
        } else if (!"redlink".equals(key)) {
          automatic = false;
        }
      }
    }
  }
}