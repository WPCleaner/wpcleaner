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
import java.util.Collection;

import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.Element;
import javax.swing.text.Style;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyleContext;
import javax.swing.text.StyledDocument;

import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElement;
import org.wikipediacleaner.api.data.PageElementCategory;
import org.wikipediacleaner.api.data.PageElementComment;
import org.wikipediacleaner.api.data.PageElementDefaultsort;
import org.wikipediacleaner.api.data.PageElementExternalLink;
import org.wikipediacleaner.api.data.PageElementImage;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.PageElementLanguageLink;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.PageElementTitle;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueStyle;


/**
 * An abstract class for formatting text in a Pane.
 */
public abstract class MWPaneFormatter {

  // Style constants
  public final static String STYLE_CHECK_WIKI_ERROR        = "CheckWikiError";
  public final static String STYLE_CHECK_WIKI_OK           = "CheckWikiOk";
  public final static String STYLE_CHECK_WIKI_WARNING      = "CheckWikiWarning";
  public final static String STYLE_DISAMBIGUATION_LINK     = "DisambiguationLink";
  public final static String STYLE_DISAMBIGUATION_TEMPLATE = "DisambiguationTemplate";
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
   * Format elements in a MediaWikiPane.
   * 
   * @param pane MediaWikiPane to be formatted.
   * @param pageAnalysis Page analysis.
   */
  public void defaultFormatElements(MWPane pane, PageAnalysis pageAnalysis) {

    // Basic checks
    if ((pane == null) || (pageAnalysis == null)) {
      return;
    }
    StyledDocument doc = pane.getStyledDocument();
    if (doc == null) {
      return;
    }

    // Retrieve configuration
    Configuration config = Configuration.getConfiguration();
    ConfigurationValueStyle.StyleProperties styleCategory = config.getStyle(
        ConfigurationValueStyle.CATEGORY);
    ConfigurationValueStyle.StyleProperties styleComments = config.getStyle(
        ConfigurationValueStyle.COMMENTS);
    ConfigurationValueStyle.StyleProperties styleDefaultsort = config.getStyle(
        ConfigurationValueStyle.DEFAULTSORT);
    ConfigurationValueStyle.StyleProperties styleExternalLink = config.getStyle(
        ConfigurationValueStyle.EXTERNAL_LINK);
    ConfigurationValueStyle.StyleProperties styleImage = config.getStyle(
        ConfigurationValueStyle.IMAGE);
    ConfigurationValueStyle.StyleProperties styleInternalLink = config.getStyle(
        ConfigurationValueStyle.INTERNAL_LINK);
    ConfigurationValueStyle.StyleProperties styleLanguageLink = config.getStyle(
        ConfigurationValueStyle.LANGUAGE_LINK);
    ConfigurationValueStyle.StyleProperties styleTemplate = config.getStyle(
        ConfigurationValueStyle.TEMPLATE);
    ConfigurationValueStyle.StyleProperties styleTitle = config.getStyle(
        ConfigurationValueStyle.TITLE);

    // Format
    Collection<PageElement> elements = pageAnalysis.getElements(
        styleCategory.getEnabled(),
        styleComments.getEnabled(),
        styleDefaultsort.getEnabled(),
        styleExternalLink.getEnabled(),
        styleImage.getEnabled(),
        styleInternalLink.getEnabled(),
        false,
        styleLanguageLink.getEnabled(),
        styleTemplate.getEnabled(),
        styleTitle.getEnabled());
    for (PageElement element : elements) {
      Style style = null;
      if (element instanceof PageElementCategory) {
        style = doc.getStyle(ConfigurationValueStyle.CATEGORY.getName());
      } else if (element instanceof PageElementComment) {
        style = doc.getStyle(ConfigurationValueStyle.COMMENTS.getName());
      } else if (element instanceof PageElementDefaultsort) {
        style = doc.getStyle(ConfigurationValueStyle.DEFAULTSORT.getName());
      } else if (element instanceof PageElementExternalLink) {
        style = doc.getStyle(ConfigurationValueStyle.EXTERNAL_LINK.getName());
      } else if (element instanceof PageElementImage) {
        style = doc.getStyle(ConfigurationValueStyle.IMAGE.getName());
      } else if (element instanceof PageElementInternalLink) {
        style = doc.getStyle(ConfigurationValueStyle.INTERNAL_LINK.getName());
      } else if (element instanceof PageElementLanguageLink) {
        style = doc.getStyle(ConfigurationValueStyle.LANGUAGE_LINK.getName());
      } else if (element instanceof PageElementTemplate) {
        style = doc.getStyle(ConfigurationValueStyle.TEMPLATE.getName());
      } else if (element instanceof PageElementTitle) {
        style = doc.getStyle(ConfigurationValueStyle.TITLE.getName());
      }
      if (style != null) {
        doc.setCharacterAttributes(
            element.getBeginIndex(),
            element.getEndIndex() - element.getBeginIndex(),
            style, true);
      }
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
   * @return A styled document that can be used to format a MWPane.
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
   * Initialize styles shared by all MWPane instances 
   */
  private static void initializeStyles() {
    synchronized (lockStyles) {
      if (!stylesInitialized) {
        Style rootStyle = styleContext.getStyle(StyleContext.DEFAULT_STYLE);

        // Style for comment
        Style categoryStyle = addStyle(ConfigurationValueStyle.CATEGORY, rootStyle);
        categoryStyle.addAttribute(ATTRIBUTE_OCCURRENCE, Boolean.FALSE);

        // Style for comment
        Style commentStyle = addStyle(ConfigurationValueStyle.COMMENTS, rootStyle);
        commentStyle.addAttribute(ATTRIBUTE_OCCURRENCE, Boolean.FALSE);

        // Style for comment
        Style defaultsortStyle = addStyle(ConfigurationValueStyle.DEFAULTSORT, rootStyle);
        defaultsortStyle.addAttribute(ATTRIBUTE_OCCURRENCE, Boolean.FALSE);

        // Style for external link
        Style externalLinkStyle = addStyle(ConfigurationValueStyle.EXTERNAL_LINK, rootStyle);
        externalLinkStyle.addAttribute(ATTRIBUTE_OCCURRENCE, Boolean.FALSE);

        // Style for image
        Style imageStyle = addStyle(ConfigurationValueStyle.IMAGE, rootStyle);
        imageStyle.addAttribute(ATTRIBUTE_OCCURRENCE, Boolean.FALSE);

        // Style for internal link
        Style internalLinkStyle = addStyle(ConfigurationValueStyle.INTERNAL_LINK, rootStyle);
        internalLinkStyle.addAttribute(ATTRIBUTE_OCCURRENCE, Boolean.FALSE);

        // Style for language link
        Style languageLinkStyle = addStyle(ConfigurationValueStyle.LANGUAGE_LINK, rootStyle);
        languageLinkStyle.addAttribute(ATTRIBUTE_OCCURRENCE, Boolean.FALSE);

        // Style for template
        Style templateStyle = addStyle(ConfigurationValueStyle.TEMPLATE, rootStyle);
        templateStyle.addAttribute(ATTRIBUTE_OCCURRENCE, Boolean.FALSE);

        // Style for title
        Style titleStyle = addStyle(ConfigurationValueStyle.TITLE, rootStyle);
        titleStyle.addAttribute(ATTRIBUTE_OCCURRENCE, Boolean.FALSE);

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

        stylesInitialized = true;
      }
    }
  }

  /**
   * @param defaultStyle Default style.
   * @param rootStyle Root style.
   * @return New style.
   */
  private static Style addStyle(ConfigurationValueStyle defaultStyle, Style rootStyle) {
    if (defaultStyle == null) {
      return null;
    }
    Style newStyle = styleContext.addStyle(defaultStyle.getName(), rootStyle);
    Configuration config = Configuration.getConfiguration();
    ConfigurationValueStyle.StyleProperties configStyle = config.getStyle(defaultStyle);
    if (configStyle != null) {
      formatStyleForeground(newStyle, config, configStyle);
      formatStyleBackground(newStyle, config, configStyle);
      formatStyleBold(newStyle, config, configStyle);
      formatStyleItalic(newStyle, config, configStyle);
      formatStyleUnderline(newStyle, config, configStyle);
      formatStyleStrikeThrough(newStyle, config, configStyle);
    }
    return newStyle;
  }

  /**
   * Modify the foreground color of a style.
   * 
   * @param style Style to be modified.
   * @param config Configuration.
   * @param configStyle Configuration of the style.
   */
  private static void formatStyleForeground(
      Style style, Configuration config,
      ConfigurationValueStyle.StyleProperties configStyle) {
    if ((style == null) || (config == null) || (configStyle == null)) {
      return;
    }
    if (!configStyle.getForeground()) {
      return;
    }
    StyleConstants.setForeground(style, configStyle.getForegroundColor());
  }

  /**
   * Modify the background color of a style.
   * 
   * @param style Style to be modified.
   * @param config Configuration.
   * @param configStyle Configuration of the style.
   */
  private static void formatStyleBackground(
      Style style, Configuration config,
      ConfigurationValueStyle.StyleProperties configStyle) {
    if ((style == null) || (config == null) || (configStyle == null)) {
      return;
    }
    if (!configStyle.getBackground()) {
      return;
    }
    StyleConstants.setBackground(style, configStyle.getBackgroundColor());
  }

  /**
   * Modify the bold attribute of a style.
   * 
   * @param style Style to be modified.
   * @param config Configuration.
   * @param configStyle Configuration of the style.
   */
  private static void formatStyleBold(
      Style style, Configuration config,
      ConfigurationValueStyle.StyleProperties configStyle) {
    StyleConstants.setBold(
        style,
        configStyle.getBold());
  }

  /**
   * Modify the italic attribute of a style.
   * 
   * @param style Style to be modified.
   * @param config Configuration.
   * @param configStyle Configuration of the style.
   */
  private static void formatStyleItalic(
      Style style, Configuration config,
      ConfigurationValueStyle.StyleProperties configStyle) {
    StyleConstants.setItalic(
        style,
        configStyle.getItalic());
  }

  /**
   * Modify the underline attribute of a style.
   * 
   * @param style Style to be modified.
   * @param config Configuration.
   * @param configStyle Configuration of the style.
   */
  private static void formatStyleUnderline(
      Style style, Configuration config,
      ConfigurationValueStyle.StyleProperties configStyle) {
    StyleConstants.setUnderline(
        style,
        configStyle.getUnderline());
  }

  /**
   * Modify the strike through attribute of a style.
   * 
   * @param style Style to be modified.
   * @param config Configuration.
   * @param configStyle Configuration of the style.
   */
  private static void formatStyleStrikeThrough(
      Style style, Configuration config,
      ConfigurationValueStyle.StyleProperties configStyle) {
    StyleConstants.setStrikeThrough(
        style,
        configStyle.getStrikeThrough());
  }
}
