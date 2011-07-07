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

import java.awt.Color;

import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.Element;
import javax.swing.text.Style;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyleContext;
import javax.swing.text.StyledDocument;

import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementComment;


/**
 * An abstract class for formatting text in a Pane.
 */
public abstract class MWPaneFormatter {

  // Style constants
  public final static String STYLE_CHECK_WIKI_ERROR        = "CheckWikiError";
  public final static String STYLE_CHECK_WIKI_OK           = "CheckWikiOk";
  public final static String STYLE_CHECK_WIKI_WARNING      = "CheckWikiWarning";
  public final static String STYLE_COMMENT                 = "Comment";
  public final static String STYLE_DISAMBIGUATION_LINK     = "DisambiguationLink";
  public final static String STYLE_DISAMBIGUATION_TEMPLATE = "DisambiguationTemplate";
  public final static String STYLE_EXTERNAL_LINK           = "ExternalLink";
  public final static String STYLE_HELP_REQUESTED_LINK     = "HelpRequestedLink";
  public final static String STYLE_MISSING_LINK            = "MissingLink";
  public final static String STYLE_NORMAL_LINK             = "NormalLink";
  public final static String STYLE_NORMAL_TEMPLATE         = "NormalTemplate";
  public final static String STYLE_REDIRECT_LINK           = "RedirectLink";

  // Attributes
  public final static String ATTRIBUTE_INFO                = "MediaWikiInfo";
  public final static String ATTRIBUTE_OCCURRENCE          = "MediaWikiOccurrence";
  public final static String ATTRIBUTE_PAGE                = "MediaWikiPage";
  public final static String ATTRIBUTE_PAGE_ELEMENT        = "MediaWikiPageElement";
  public final static String ATTRIBUTE_TEMPLATE_MATCHER    = "MediaWikiTemplateMatcher";
  public final static String ATTRIBUTE_TEXT                = "MediaWikiText";
  public final static String ATTRIBUTE_TYPE                = "MediaWikiType";
  public final static String ATTRIBUTE_UUID                = "MediaWikiUUID";

  // Attributes values
  public final static String VALUE_CHECK_WIKI_ERROR        = "CheckWikiError";
  public final static String VALUE_CHECK_WIKI_OK           = "CheckWikiOk";
  public final static String VALUE_CHECK_WIKI_WARNING      = "CheckWikiWarning";
  public final static String VALUE_DISAMBIGUATION_LINK     = "DisambiguationLink";
  public final static String VALUE_DISAMBIGUATION_TEMPLATE = "DisambiguationTemplate";
  public final static String VALUE_EXTERNAL_LINK           = "ExternalLink";
  public final static String VALUE_HELP_REQUESTED_LINK     = "HelpRequestedLink";
  public final static String VALUE_MISSING_LINK            = "MissingLink";
  public final static String VALUE_NORMAL_LINK             = "NormalLink";
  public final static String VALUE_NORMAL_TEMPLATE         = "NormalLink";
  public final static String VALUE_REDIRECT_LINK           = "RedirectLink";

  /**
   * Constructor of MediaWikiPane formatter.
   */
  public MWPaneFormatter() {
    initializeStyles();
  }

  /* ======================================================================== */
  /* Document formatting                                                      */
  /* ======================================================================== */

  /**
   * Clean format of a MediaWikiPane.
   * 
   * @param pane MediaWikiPane to be formatted.
   */
  public void cleanFormat(MWPane pane) {

    // Basic verifications
    if (pane == null) {
      return;
    }
    StyledDocument doc = pane.getStyledDocument();
    if (doc == null) {
      return;
    }

    // Clean formation element by element
    int length = doc.getLength();
    int lastEnd = Integer.MAX_VALUE;
    for (int pos = 0; pos < length; pos = lastEnd) {
      Element run = doc.getCharacterElement(pos);
      lastEnd = run.getEndOffset();
      if (pos == lastEnd) {
        // offset + length beyond length of document, bail.
        break;
      }
      doc.setCharacterAttributes(
          run.getStartOffset(),
          run.getEndOffset() - run.getStartOffset(),
          doc.getStyle(StyleContext.DEFAULT_STYLE),
          true);
    }
  }

  /**
   * Format comments in a MediaWikiPane.
   * 
   * @param pane MediaWikiPane to be formatted.
   * @param pageAnalysis Page analysis.
   */
  public void formatComments(MWPane pane, PageAnalysis pageAnalysis) {
    if ((pane == null) || (pageAnalysis == null)) {
      return;
    }
    StyledDocument doc = pane.getStyledDocument();
    if (doc == null) {
      return;
    }
    for (PageElementComment comment : pageAnalysis.getComments()) {
      doc.setCharacterAttributes(
          comment.getBeginIndex(),
          comment.getEndIndex() - comment.getBeginIndex(),
          doc.getStyle(STYLE_COMMENT),
          true);
    }
  }

  /**
   * Format text in a MediaWikiPane.
   * 
   * @param pane MediaWikiPane to be formatted.
   * @param pageAnalysis Page analysis.
   */
  public abstract void format(MWPane pane, PageAnalysis pageAnalysis);

  /* ======================================================================== */
  /* Document management                                                      */
  /* ======================================================================== */

  /**
   * @return A styled document that can be used to format a MediaWikiPane.
   */
  public static StyledDocument createDocument() {
    initializeStyles();
    StyledDocument document = new DefaultStyledDocument(styleContext);
    return document;
  }

  /* ======================================================================== */
  /* Style management                                                         */
  /* ======================================================================== */

  private final static Object lockStyles = new Object();
  private final static StyleContext styleContext = new StyleContext();
  private static boolean stylesInitialized = false;

  /**
   * Initialize styles shared by all MediaWikiPane instances 
   */
  private static void initializeStyles() {
    synchronized (lockStyles) {
      if (!stylesInitialized) {
        Style rootStyle = styleContext.getStyle(StyleContext.DEFAULT_STYLE);

        // Style for comment
        Style commentStyle = styleContext.addStyle(STYLE_COMMENT, rootStyle);
        StyleConstants.setForeground(commentStyle, Color.GRAY);
        StyleConstants.setItalic(commentStyle, true);
        commentStyle.addAttribute(ATTRIBUTE_OCCURRENCE, Boolean.FALSE);

        // Style for normal link
        Style normalLinkStyle = styleContext.addStyle(STYLE_NORMAL_LINK, rootStyle);
        StyleConstants.setBold(normalLinkStyle, true);
        StyleConstants.setForeground(normalLinkStyle, Color.BLUE);
        normalLinkStyle.addAttribute(ATTRIBUTE_TYPE, VALUE_NORMAL_LINK);

        // Style for normal template
        Style normalTemplateStyle = styleContext.addStyle(STYLE_NORMAL_TEMPLATE, rootStyle);
        StyleConstants.setBold(normalTemplateStyle, true);
        StyleConstants.setForeground(normalTemplateStyle, Color.BLUE);
        normalTemplateStyle.addAttribute(ATTRIBUTE_TYPE, VALUE_NORMAL_TEMPLATE);

        // Style for CheckWiki error
        Style checkWikiErrorStyle = styleContext.addStyle(STYLE_CHECK_WIKI_ERROR, rootStyle);
        StyleConstants.setBold(checkWikiErrorStyle, true);
        StyleConstants.setForeground(checkWikiErrorStyle, Color.RED);
        checkWikiErrorStyle.addAttribute(ATTRIBUTE_TYPE, VALUE_CHECK_WIKI_ERROR);

        // Style for CheckWiki OK
        Style checkWikiOkStyle = styleContext.addStyle(STYLE_CHECK_WIKI_OK, rootStyle);
        StyleConstants.setBold(checkWikiOkStyle, true);
        StyleConstants.setForeground(checkWikiOkStyle, Color.GREEN);
        checkWikiOkStyle.addAttribute(ATTRIBUTE_TYPE, VALUE_CHECK_WIKI_OK);
        checkWikiOkStyle.addAttribute(ATTRIBUTE_OCCURRENCE, Boolean.FALSE);

        // Style for CheckWiki warning
        Style checkWikiWarningStyle = styleContext.addStyle(STYLE_CHECK_WIKI_WARNING, rootStyle);
        StyleConstants.setBold(checkWikiWarningStyle, true);
        StyleConstants.setForeground(checkWikiWarningStyle, Color.ORANGE);
        checkWikiWarningStyle.addAttribute(ATTRIBUTE_TYPE, VALUE_CHECK_WIKI_WARNING);

        // Style for disambiguation link 
        Style disambiguationLinkStyle = styleContext.addStyle(STYLE_DISAMBIGUATION_LINK, rootStyle);
        StyleConstants.setBold(disambiguationLinkStyle, true);
        StyleConstants.setForeground(disambiguationLinkStyle, Color.RED);
        disambiguationLinkStyle.addAttribute(ATTRIBUTE_TYPE, VALUE_DISAMBIGUATION_LINK);

        // Style for disambiguation template
        Style disambiguationTemplateStyle = styleContext.addStyle(STYLE_DISAMBIGUATION_TEMPLATE, rootStyle);
        StyleConstants.setBold(disambiguationTemplateStyle, true);
        StyleConstants.setForeground(disambiguationTemplateStyle, Color.RED);
        disambiguationTemplateStyle.addAttribute(ATTRIBUTE_TYPE, VALUE_DISAMBIGUATION_TEMPLATE);

        // Style for help requested link
        Style helpRequestedLinkStyle = styleContext.addStyle(STYLE_HELP_REQUESTED_LINK, rootStyle);
        StyleConstants.setBold(helpRequestedLinkStyle, true);
        StyleConstants.setForeground(helpRequestedLinkStyle, Color.ORANGE);
        helpRequestedLinkStyle.addAttribute(ATTRIBUTE_TYPE, VALUE_HELP_REQUESTED_LINK);

        // Style for redirect link
        Style redirectLinkStyle = styleContext.addStyle(STYLE_REDIRECT_LINK, rootStyle);
        StyleConstants.setBold(redirectLinkStyle, true);
        StyleConstants.setItalic(redirectLinkStyle, true);
        StyleConstants.setForeground(redirectLinkStyle, Color.CYAN);
        redirectLinkStyle.addAttribute(ATTRIBUTE_TYPE, VALUE_REDIRECT_LINK);

        // Style for missing link
        Style missingLinkStyle = styleContext.addStyle(STYLE_MISSING_LINK, rootStyle);
        StyleConstants.setBold(missingLinkStyle, true);
        StyleConstants.setForeground(missingLinkStyle, Color.ORANGE);
        StyleConstants.setStrikeThrough(missingLinkStyle, true);
        missingLinkStyle.addAttribute(ATTRIBUTE_TYPE, VALUE_MISSING_LINK);

        // Style for external link
        Style externalLinkStyle = styleContext.addStyle(STYLE_EXTERNAL_LINK, rootStyle);
        StyleConstants.setForeground(externalLinkStyle, new Color(128, 128, 255));
        externalLinkStyle.addAttribute(ATTRIBUTE_TYPE, VALUE_EXTERNAL_LINK);

        stylesInitialized = true;
      }
    }
  }
}
