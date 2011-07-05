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
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.TemplateMatcher;


/**
 * A disambiguation formatter for MediaWikiPane.
 */
public class MediaWikiPaneDisambiguationFormatter extends
    MediaWikiPaneFormatter {

  private final EnumWikipedia wikipedia;

  /**
   * Construct a disambiguation formatter.
   * 
   * @param wikipedia Wikipedia.
   * @param links Links of interest.
   */
  public MediaWikiPaneDisambiguationFormatter(
      EnumWikipedia wikipedia, List<Page> links) {
    this.wikipedia = wikipedia;
    this.links = links;
  }

  /**
   * Format text in a MediaWikiPane.
   * 
   * @param pane MediaWikiPane to be formatted.
   * @param pageAnalysis Page analysis.
   */
  @Override
  public void format(MediaWikiPane pane, PageAnalysis pageAnalysis) {
    // Clean formatting
    cleanFormat(pane);

    // Reset caret informations
    resetCaretPosition();

    // Format comments
    formatComments(pane, pageAnalysis);

    // Format internal links
    formatInternalLinks(pane, pageAnalysis);

    // Format templates
    formatTemplates(pane, pageAnalysis);

    // Move caret
    moveCaret(pane);
  }

  /**
   * Format internal links in a MediaWikiPane.
   * 
   * @param pane MediaWikiPane to be formatted.
   * @param pageAnalysis Page analysis.
   */
  private void formatInternalLinks(
      MediaWikiPane pane, PageAnalysis pageAnalysis) {
    if ((pane == null) || (pageAnalysis == null)) {
      return;
    }
    for (PageElementInternalLink link : pageAnalysis.getInternalLinks()) {
      formatInternalLink(pane, link);
    }
  }

  /**
   * Format an internal link in a MediaWikiPane.
   * 
   * @param pane MediaWikiPane to be formatted.
   * @param internalLink Internal link to be formatted.
   */
  private void formatInternalLink(
      MediaWikiPane pane, PageElementInternalLink internalLink) {

    // Basic verifications
    if ((pane == null) || (internalLink == null)) {
      return;
    }

    // Check if the link should be formatted
    Page link = findPage(internalLink.getLink());
    if (link == null) {
      return;
    }

    // Format the link
    boolean disambiguation = Boolean.TRUE.equals(link.isDisambiguationPage());
    String styleName = disambiguation ?
        STYLE_DISAMBIGUATION_LINK :
        link.isRedirect() ?
            STYLE_REDIRECT_LINK :
            link.isExisting() ?
                STYLE_NORMAL_LINK :
                STYLE_MISSING_LINK;
    Style attr = pane.getStyle(styleName);
    StyledDocument doc = pane.getStyledDocument();
    if ((doc == null) || (attr == null)) {
      return;
    }
    attr = (Style) attr.copyAttributes();
    attr.addAttribute(ATTRIBUTE_PAGE, link);
    attr.addAttribute(ATTRIBUTE_TEXT, internalLink.getDisplayedText());
    attr.addAttribute(ATTRIBUTE_UUID, UUID.randomUUID());
    int start = internalLink.getBeginIndex();
    int end = internalLink.getEndIndex();
    doc.setCharacterAttributes(start, end - start, attr, true);
    if (start < startPosition) {
      startPosition = start;
      endPosition = end;
    }
  }

  /**
   * Format templates in a MediaWikiPane.
   * 
   * @param pane MediaWikiPane to be formatted.
   * @param pageAnalysis Page analysis.
   */
  private void formatTemplates(
      MediaWikiPane pane, PageAnalysis pageAnalysis) {
    if (pageAnalysis == null) {
      return;
    }
    for (PageElementTemplate template : pageAnalysis.getTemplates()) {
      formatTemplate(pane, pageAnalysis, template);
    }
  }

  /**
   * Format a template in a MediaWikiPane.
   * 
   * @param pane MediaWikiPane to be formatted.
   * @param pageAnalysis Page analysis.
   * @param template Template to be formatted.
   */
  private void formatTemplate(
      MediaWikiPane pane, PageAnalysis pageAnalysis,
      PageElementTemplate template) {

    // Basic verifications
    if ((pane == null) || (pageAnalysis == null) || (template == null) || (links == null)) {
      return;
    }

    // Check if the link should be formatted
    String templateName = template.getTemplateName();
    List<? extends TemplateMatcher> matchers = wikipedia.getTemplateMatchers(templateName);
    if (matchers == null) {
      return;
    }
    for (TemplateMatcher matcher : matchers) {
      String linkTo = matcher.linksTo(pageAnalysis.getPage(), template);
      if (linkTo != null) {
        Page link = findPage(linkTo);
        if (link != null) {
          formatTemplate(pane, link, template, matcher);
          return;
        }
      }
    }
  }

  /**
   * Format a template in a MediaWikiPane.
   * 
   * @param pane MediaWikiPane to be formatted.
   * @param link Page linked.
   * @param template Template to be formatted.
   * @param matcher Template matcher.
   */
  private void formatTemplate(
      MediaWikiPane pane,
      Page link,
      PageElementTemplate template,
      TemplateMatcher matcher) {

    // Basic verifications
    if ((pane == null) || (template == null) || (matcher == null)) {
      return;
    }

    // Format template
    int start = template.getBeginIndex();
    int end = template.getEndIndex();
    String styleName = null;
    if (matcher.isGood() || Boolean.FALSE.equals(link.isDisambiguationPage())) {
      styleName = STYLE_NORMAL_TEMPLATE;
    } else {
      if (matcher.isHelpNeeded()) {
        styleName = STYLE_HELP_REQUESTED_LINK;
      } else {
        styleName = STYLE_DISAMBIGUATION_TEMPLATE;
      }
    }
    Style attr = pane.getStyle(styleName);
    attr = (Style) attr.copyAttributes();
    attr.addAttribute(ATTRIBUTE_PAGE, link);
    attr.addAttribute(ATTRIBUTE_PAGE_ELEMENT, template);
    attr.addAttribute(ATTRIBUTE_TEMPLATE_MATCHER, matcher);
    attr.addAttribute(ATTRIBUTE_UUID, UUID.randomUUID());
    if ((matcher.isHelpNeeded()) && (template.getParameterCount() > 0)) {
      attr.addAttribute(
          ATTRIBUTE_TEXT,
          (template.getParameterCount() > 1) ?
              template.getParameterValue(1) : template.getParameterValue(0));
    }
    StyledDocument doc = pane.getStyledDocument();
    doc.setCharacterAttributes(start, end - start, attr, true);
    if (matcher.isGood() || !Boolean.FALSE.equals(link.isDisambiguationPage())) {
      if (start < thirdStartPosition) {
        thirdStartPosition = start;
        thirdEndPosition = end;
      }
    } else if (matcher.isHelpNeeded()) {
      if (start < secondStartPosition) {
        secondStartPosition = start;
        secondEndPosition = end;
      }
    } else {
      if (start < startPosition) {
        startPosition = start;
        endPosition = end;
      }
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
  private void moveCaret(MediaWikiPane pane) {
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
