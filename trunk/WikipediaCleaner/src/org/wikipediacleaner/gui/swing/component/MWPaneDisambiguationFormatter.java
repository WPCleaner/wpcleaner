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

package org.wikipediacleaner.gui.swing.component;

import java.util.List;
import java.util.UUID;

import javax.swing.text.Style;
import javax.swing.text.StyledDocument;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.constants.WPCConfigurationStringList;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.TemplateMatcher;
import org.wikipediacleaner.utils.ConfigurationValueStyle;


/**
 * A disambiguation formatter for MediaWikiPane.
 */
public class MWPaneDisambiguationFormatter extends
    MWPaneFormatter {

  private final EnumWikipedia wikipedia;

  /**
   * Construct a disambiguation formatter.
   * 
   * @param wikipedia Wikipedia.
   * @param links Links of interest.
   */
  public MWPaneDisambiguationFormatter(
      EnumWikipedia wikipedia, List<Page> links) {
    this.wikipedia = wikipedia;
    this.links = links;
  }

  /**
   * Format text in a StyleDocument.
   * 
   * @param doc Document to be formatted.
   * @param pageAnalysis Page analysis.
   */
  @Override
  public void format(StyledDocument doc, PageAnalysis pageAnalysis) {

    // Clean formatting
    cleanFormat(doc);

    // Reset caret informations
    resetCaretPosition();

    // Format comments
    defaultFormatElements(doc, pageAnalysis);

    // Format internal links
    formatInternalLinks(doc, pageAnalysis);

    // Format templates
    formatTemplates(doc, pageAnalysis);
  }

  /**
   * Format internal links in a MediaWikiPane.
   * 
   * @param doc Document to be formatted.
   * @param pageAnalysis Page analysis.
   */
  private void formatInternalLinks(
      StyledDocument doc,
      PageAnalysis pageAnalysis) {
    if ((doc == null) || (pageAnalysis == null)) {
      return;
    }
    for (PageElementInternalLink link : pageAnalysis.getInternalLinks()) {
      formatInternalLink(doc, link, pageAnalysis);
    }
  }

  /**
   * Format an internal link in a MediaWikiPane.
   * 
   * @param doc Document to be formatted.
   * @param internalLink Internal link to be formatted.
   * @param pageAnalysis Page analysis.
   */
  private void formatInternalLink(
      StyledDocument doc,
      PageElementInternalLink internalLink,
      PageAnalysis pageAnalysis) {

    // Basic verifications
    if ((doc == null) || (internalLink == null)) {
      return;
    }

    // Check if the link should be formatted
    Page link = findPage(internalLink.getLink());
    if (link == null) {
      return;
    }

    // Format the link
    boolean disambiguation = Boolean.TRUE.equals(link.isDisambiguationPage());
    int start = internalLink.getBeginIndex();
    int end = internalLink.getEndIndex();
    ConfigurationValueStyle styleType = null;
    if (disambiguation) {
      styleType = ConfigurationValueStyle.INTERNAL_LINK_DAB;
      List<String> templatesAfter = wikipedia.getConfiguration().getStringList(
          WPCConfigurationStringList.TEMPLATES_AFTER_HELP_ASKED);
      if ((templatesAfter != null) && (templatesAfter.size() > 0)) {
        String contents = pageAnalysis.getContents();
        int maxSize = contents.length();
        int currentPos = end;
        while ((currentPos < maxSize) && (contents.charAt(currentPos) == ' ')) {
          currentPos++;
        }
        if ((currentPos < maxSize) && (contents.charAt(currentPos) == '{')) {
          PageElementTemplate nextTemplate = pageAnalysis.isInTemplate(currentPos);
          if (nextTemplate != null) {
            for (String templateAfter : templatesAfter) {
              if (Page.areSameTitle(templateAfter, nextTemplate.getTemplateName())) {
                styleType = ConfigurationValueStyle.HELP_REQUESTED;
                end = nextTemplate.getEndIndex();
              }
            }
          }
        }
      }
    } else if (link.isRedirect()) {
      styleType = ConfigurationValueStyle.INTERNAL_LINK_REDIRECT;
    } else if (Boolean.TRUE.equals(link.isExisting())) {
      styleType = ConfigurationValueStyle.INTERNAL_LINK_NORMAL;
    } else {
      styleType = ConfigurationValueStyle.INTERNAL_LINK_MISSING;
    }
    Style attr = doc.getStyle(styleType.getName());
    if (attr == null) {
      return;
    }
    attr = (Style) attr.copyAttributes();
    attr.addAttribute(ATTRIBUTE_PAGE, link);
    attr.addAttribute(ATTRIBUTE_TEXT, internalLink.getDisplayedText());
    attr.addAttribute(ATTRIBUTE_UUID, UUID.randomUUID());
    doc.setCharacterAttributes(start, end - start, attr, true);
    if (start < startPosition) {
      startPosition = start;
      endPosition = end;
    }
  }

  /**
   * Format templates in a MediaWikiPane.
   * 
   * @param doc Document to be formatted.
   * @param pageAnalysis Page analysis.
   */
  private void formatTemplates(
      StyledDocument doc,
      PageAnalysis pageAnalysis) {
    if (pageAnalysis == null) {
      return;
    }
    for (PageElementTemplate template : pageAnalysis.getTemplates()) {
      formatTemplate(doc, pageAnalysis, template);
    }
  }

  /**
   * Format a template in a MediaWikiPane.
   * 
   * @param doc Document to be formatted.
   * @param pageAnalysis Page analysis.
   * @param template Template to be formatted.
   */
  private void formatTemplate(
      StyledDocument doc,
      PageAnalysis pageAnalysis,
      PageElementTemplate template) {

    // Basic verifications
    if ((doc == null) ||
        (pageAnalysis == null) || (template == null) || (links == null)) {
      return;
    }

    // Check if the link should be formatted
    String templateName = template.getTemplateName();
    List<? extends TemplateMatcher> matchers =
        wikipedia.getConfiguration().getTemplateMatchers(templateName);
    if (matchers == null) {
      return;
    }
    for (TemplateMatcher matcher : matchers) {
      String linkTo = matcher.linksTo(pageAnalysis.getPage(), template);
      if (linkTo != null) {
        Page link = findPage(linkTo);
        if (link != null) {
          formatTemplate(doc, link, template, matcher);
          return;
        }
      }
    }
  }

  /**
   * Format a template in a MediaWikiPane.
   * 
   * @param doc Document to be formatted.
   * @param link Page linked.
   * @param template Template to be formatted.
   * @param matcher Template matcher.
   */
  private void formatTemplate(
      StyledDocument doc,
      Page link,
      PageElementTemplate template,
      TemplateMatcher matcher) {

    // Basic verifications
    if ((doc == null) ||
        (template == null) || (matcher == null)) {
      return;
    }

    // Format template
    if (matcher.isGood() || Boolean.FALSE.equals(link.isDisambiguationPage())) {
      formatTemplateGood(doc, link, template, matcher);
    } else if (matcher.isHelpNeeded()) {
      formatTemplateHelpRequested(doc, link, template, matcher);
    } else {
      formatTemplateDisambiguation(doc, link, template, matcher);
    }
  }

  /**
   * Format a template in a MediaWikiPane.
   * 
   * @param doc Document to be formatted.
   * @param link Page linked.
   * @param template Template to be formatted.
   * @param matcher Template matcher.
   */
  private void formatTemplateGood(
      StyledDocument doc,
      Page link,
      PageElementTemplate template,
      TemplateMatcher matcher) {

    // Basic verifications
    if ((doc == null) ||
        (template == null) || (matcher == null)) {
      return;
    }

    // Format template
    int start = template.getBeginIndex();
    int end = template.getEndIndex();
    Style attr = doc.getStyle(ConfigurationValueStyle.TEMPLATE_NORMAL.getName());
    attr = (Style) attr.copyAttributes();
    attr.addAttribute(ATTRIBUTE_PAGE, link);
    attr.addAttribute(ATTRIBUTE_PAGE_ELEMENT, template);
    attr.addAttribute(ATTRIBUTE_TEMPLATE_MATCHER, matcher);
    attr.addAttribute(ATTRIBUTE_UUID, UUID.randomUUID());
    doc.setCharacterAttributes(start, end - start, attr, true);
    if (start < thirdStartPosition) {
      thirdStartPosition = start;
      thirdEndPosition = end;
    }
  }

  /**
   * Format a template in a MediaWikiPane.
   * 
   * @param doc Document to be formatted.
   * @param link Page linked.
   * @param template Template to be formatted.
   * @param matcher Template matcher.
   */
  private void formatTemplateHelpRequested(
      StyledDocument doc,
      Page link,
      PageElementTemplate template,
      TemplateMatcher matcher) {

    // Basic verifications
    if ((doc == null) || (template == null) || (matcher == null)) {
      return;
    }

    // Format template
    int start = template.getBeginIndex();
    int end = template.getEndIndex();
    Style attr = doc.getStyle(ConfigurationValueStyle.HELP_REQUESTED.getName());
    attr = (Style) attr.copyAttributes();
    attr.addAttribute(ATTRIBUTE_PAGE, link);
    attr.addAttribute(ATTRIBUTE_UUID, UUID.randomUUID());
    if (template.getParameterCount() > 0) {
      attr.addAttribute(
          ATTRIBUTE_TEXT,
          (template.getParameterCount() > 1) ?
              template.getParameterValue(1) : template.getParameterValue(0));
    } else {
      attr.addAttribute(ATTRIBUTE_PAGE_ELEMENT, template);
      attr.addAttribute(ATTRIBUTE_TEMPLATE_MATCHER, matcher);
    }
    doc.setCharacterAttributes(start, end - start, attr, true);
    if (start < secondStartPosition) {
      secondStartPosition = start;
      secondEndPosition = end;
    }
  }

  /**
   * Format a template in a MediaWikiPane.
   * 
   * @param doc Document to be formatted.
   * @param link Page linked.
   * @param template Template to be formatted.
   * @param matcher Template matcher.
   */
  private void formatTemplateDisambiguation(
      StyledDocument doc,
      Page link,
      PageElementTemplate template,
      TemplateMatcher matcher) {

    // Basic verifications
    if ((doc == null) || (template == null) || (matcher == null)) {
      return;
    }

    // Format template
    int start = template.getBeginIndex();
    int end = template.getEndIndex();
    Style attr = doc.getStyle(ConfigurationValueStyle.TEMPLATE_DAB.getName());
    attr = (Style) attr.copyAttributes();
    attr.addAttribute(ATTRIBUTE_PAGE, link);
    attr.addAttribute(ATTRIBUTE_PAGE_ELEMENT, template);
    attr.addAttribute(ATTRIBUTE_TEMPLATE_MATCHER, matcher);
    attr.addAttribute(ATTRIBUTE_UUID, UUID.randomUUID());
    doc.setCharacterAttributes(start, end - start, attr, true);
    if (start < startPosition) {
      startPosition = start;
      endPosition = end;
    }
  }

  /* ======================================================================== */
  /* Caret management                                                         */
  /* ======================================================================== */

  private int startPosition;
  private int endPosition;
  private int secondStartPosition;
  private int secondEndPosition;
  private int thirdStartPosition;
  private int thirdEndPosition;

  /**
   * Reset caret positions. 
   */
  private void resetCaretPosition() {
    startPosition = Integer.MAX_VALUE;
    endPosition = Integer.MAX_VALUE;
    secondStartPosition = Integer.MAX_VALUE;
    secondEndPosition = Integer.MAX_VALUE;
    thirdStartPosition = Integer.MAX_VALUE;
    thirdEndPosition = Integer.MAX_VALUE;
  }

  /**
   * Move caret.
   * 
   * @param pane MediaWikiPane.
   */
  @Override
  protected void moveCaret(MWPane pane) {
    if (pane == null) {
      return;
    }

    if (startPosition < Integer.MAX_VALUE) {
      pane.setCaretPosition(startPosition);
      pane.moveCaretPosition(endPosition);
    } else if (secondStartPosition < Integer.MAX_VALUE) {
      pane.setCaretPosition(secondStartPosition);
      pane.moveCaretPosition(secondEndPosition);
    } else if (thirdStartPosition < Integer.MAX_VALUE) {
      pane.setCaretPosition(thirdStartPosition);
      pane.moveCaretPosition(thirdEndPosition);
    }
  }

  /* ======================================================================== */
  /* Links management                                                         */
  /* ======================================================================== */

  /**
   * List of links of interest. 
   */
  private final List<Page> links;

  /**
   * Find the page matching a page name.
   * 
   * @param pagename Page name.
   * @return Page matching the page name.
   */
  private Page findPage(String pagename) {
    if (links != null) {
      for (Page page : links) {
        if (Page.areSameTitle(page.getTitle(), pagename)) {
          return page;
        }
      }
    }
    return null;
  }

  /**
   * @param pages List of pages.
   * @return True if the list is identical.
   */
  public boolean isSameList(List<Page> pages) {
    if ((pages == null) || (links == null)) {
      return false;
    }
    return pages.equals(links);
  }
}
