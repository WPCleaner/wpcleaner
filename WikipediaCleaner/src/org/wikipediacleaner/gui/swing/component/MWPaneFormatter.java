/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.component;

import java.util.List;

import javax.swing.JTextPane;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.Element;
import javax.swing.text.Style;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyleContext;
import javax.swing.text.StyledDocument;

import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElement;
import org.wikipediacleaner.api.data.PageElementCategory;
import org.wikipediacleaner.api.data.PageElementComment;
import org.wikipediacleaner.api.data.PageElementExternalLink;
import org.wikipediacleaner.api.data.PageElementFunction;
import org.wikipediacleaner.api.data.PageElementImage;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.PageElementInterwikiLink;
import org.wikipediacleaner.api.data.PageElementLanguageLink;
import org.wikipediacleaner.api.data.PageElementMagicWord;
import org.wikipediacleaner.api.data.PageElementParameter;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.PageElementTitle;
import org.wikipediacleaner.api.data.PageElementAreas.Area;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueInteger;
import org.wikipediacleaner.utils.ConfigurationValueStyle;


/**
 * An abstract class for formatting text in a Pane.
 */
public abstract class MWPaneFormatter {

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

  // ==========================================================================
  // Document formatting
  // ==========================================================================

  /**
   * Clean format of a MediaWikiPane.
   * 
   * @param doc Styled document to be formatted.
   */
  protected void cleanFormat(StyledDocument doc) {

    // Basic verifications
    if (doc == null) {
      return;
    }

    // Clean formation element by element
    doc.setCharacterAttributes(
        0, doc.getLength(),
        doc.getStyle(StyleContext.DEFAULT_STYLE),
        true);
  }

  /**
   * Move caret.
   * 
   * @param pane MediaWikiPane.
   */
  protected abstract void moveCaret(MWPane pane);

  /**
   * Replace the default formatting by highlighting non wiki text.
   */
  private final boolean showNonWikiText = false;

  /**
   * Format elements in a MediaWikiPane.
   * 
   * @param doc Document to be formatted.
   * @param analysis Page analysis.
   */
  public void defaultFormatElements(
      StyledDocument doc,
      PageAnalysis analysis) {
    if (showNonWikiText) {
      formatWikiText(doc, analysis);
    } else {
      formatWikiSyntax(doc, analysis);
    }
  }

  /**
   * Format elements in a MediaWikiPane following Wiki syntax.
   * 
   * @param doc Document to be formatted.
   * @param analysis Page analysis.
   */
  private void formatWikiSyntax(
      StyledDocument doc,
      PageAnalysis analysis) {
    // Basic checks
    if ((doc == null) || (analysis == null)) {
      return;
    }

    // Retrieve configuration
    Configuration config = Configuration.getConfiguration();
    int limit = config.getInt(null, ConfigurationValueInteger.SYNTAX_HIGHLIGHTING_LIMIT);
    if (doc.getLength() > limit) {
      return;
    }
    ConfigurationValueStyle.StyleProperties styleCategory = config.getStyle(
        ConfigurationValueStyle.CATEGORY);
    ConfigurationValueStyle.StyleProperties styleComments = config.getStyle(
        ConfigurationValueStyle.COMMENTS);
    ConfigurationValueStyle.StyleProperties styleExternalLink = config.getStyle(
        ConfigurationValueStyle.EXTERNAL_LINK);
    ConfigurationValueStyle.StyleProperties styleImage = config.getStyle(
        ConfigurationValueStyle.IMAGE);
    ConfigurationValueStyle.StyleProperties styleInternalLink = config.getStyle(
        ConfigurationValueStyle.INTERNAL_LINK);
    ConfigurationValueStyle.StyleProperties styleInterwikiLink = config.getStyle(
        ConfigurationValueStyle.INTERWIKI_LINK);
    ConfigurationValueStyle.StyleProperties styleLanguageLink = config.getStyle(
        ConfigurationValueStyle.LANGUAGE_LINK);
    ConfigurationValueStyle.StyleProperties styleProgramming = config.getStyle(
        ConfigurationValueStyle.PROGRAMMING);
    ConfigurationValueStyle.StyleProperties styleTag = config.getStyle(
        ConfigurationValueStyle.TAG);
    ConfigurationValueStyle.StyleProperties styleTemplate = config.getStyle(
        ConfigurationValueStyle.TEMPLATE);
    ConfigurationValueStyle.StyleProperties styleTitle = config.getStyle(
        ConfigurationValueStyle.TITLE);

    // Format
    List<PageElement> elements = analysis.getElements(
        styleCategory.getEnabled(),
        styleComments.getEnabled(),
        styleExternalLink.getEnabled(),
        styleProgramming.getEnabled(),
        styleImage.getEnabled(),
        styleInternalLink.getEnabled(),
        styleInterwikiLink.getEnabled(),
        styleLanguageLink.getEnabled(),
        styleProgramming.getEnabled(),
        styleProgramming.getEnabled(),
        styleTag.getEnabled(),
        styleTemplate.getEnabled(),
        styleTitle.getEnabled());
    formatElementsDirectly(doc, analysis, elements, 0, elements.size());
  }

  /**
   * Format elements in a MediaWikiPane highlighting non wiki text.
   * 
   * @param doc Document to be formatted.
   * @param analysis Page analysis.
   */
  private void formatWikiText(
      StyledDocument doc,
      PageAnalysis analysis) {
    if ((doc == null) || (analysis == null)) {
      return;
    }
    Style style = doc.getStyle(ConfigurationValueStyle.COMMENTS.getName());
    List<Area> areas = analysis.getAreas().getAreas();
    if (areas != null) {
      for (Area area : areas) {
        int beginIndex = area.getBeginIndex();
        int endIndex = area.getEndIndex();
        doc.setCharacterAttributes(
            beginIndex, endIndex - beginIndex,
            style, true);
      }
    }
  }

  /**
   * Format a list of elements in a document.
   * 
   * @param doc Document.
   * @param analysis Page analysis.
   * @param elements Elements.
   * @param begin Begin index in the elements.
   * @param end End index in the elements.
   */
  private void formatElementsDirectly(
      StyledDocument doc,
      PageAnalysis analysis,
      List<PageElement> elements,
      int begin, int end) {
    for (int i = begin; i < end; i++) {
      PageElement element = elements.get(i);
      int beginIndex = element.getBeginIndex();
      int endIndex = element.getEndIndex();
      Style style = null;
      if (element instanceof PageElementCategory) {
        style = doc.getStyle(ConfigurationValueStyle.CATEGORY.getName());
      } else if (element instanceof PageElementComment) {
        style = doc.getStyle(ConfigurationValueStyle.COMMENTS.getName());
      } else if (element instanceof PageElementExternalLink) {
        style = doc.getStyle(ConfigurationValueStyle.EXTERNAL_LINK.getName());
      } else if (element instanceof PageElementInterwikiLink) {
        style = doc.getStyle(ConfigurationValueStyle.INTERWIKI_LINK.getName());
      } else if (element instanceof PageElementFunction) {
        style = doc.getStyle(ConfigurationValueStyle.PROGRAMMING.getName());
      } else if (element instanceof PageElementMagicWord) {
        style = doc.getStyle(ConfigurationValueStyle.PROGRAMMING.getName());
      } else if (element instanceof PageElementImage) {
        style = doc.getStyle(ConfigurationValueStyle.IMAGE.getName());
      } else if (element instanceof PageElementInternalLink) {
        style = getInternalLinkStyle(doc, analysis, (PageElementInternalLink) element);
      } else if (element instanceof PageElementLanguageLink) {
        style = doc.getStyle(ConfigurationValueStyle.LANGUAGE_LINK.getName());
      } else if (element instanceof PageElementParameter) {
        style = doc.getStyle(ConfigurationValueStyle.PROGRAMMING.getName());
      } else if (element instanceof PageElementTag) {
        PageElementTag tag = (PageElementTag) element;
        String name = tag.getNormalizedName();
        if (PageElementTag.TAG_WIKI_REF.equals(name) ||
            PageElementTag.TAG_WIKI_REFERENCES.equals(name)) {
          style = doc.getStyle(ConfigurationValueStyle.REFERENCE.getName());
          if (style != null) {
            endIndex = tag.getCompleteEndIndex();
          }
        }
        if (style == null) {
          style = doc.getStyle(ConfigurationValueStyle.TAG.getName());
        }
      } else if (element instanceof PageElementTemplate) {
        style = doc.getStyle(ConfigurationValueStyle.TEMPLATE.getName());
      } else if (element instanceof PageElementTitle) {
        style = doc.getStyle(ConfigurationValueStyle.TITLE.getName());
      }
      if (style != null) {
        doc.setCharacterAttributes(
            beginIndex, endIndex - beginIndex,
            style, true);
      }
    }
  }

  private Style getInternalLinkStyle(
      StyledDocument doc,
      PageAnalysis analysis,
      PageElementInternalLink link) {
    Style style = null;

    if ((analysis != null) &&
        (analysis.getPage() != null) &&
        (analysis.getPage().getLinks() != null)) {

      // Find link target
      Page target = null;
      for (Page tmpPage : analysis.getPage().getLinks()) {
        if (Page.areSameTitle(tmpPage.getTitle(), link.getLink())) {
          target = tmpPage;
        }
      }

      // Specific styles
      if (target != null) {
        if (target.isRedirect()) {
          style = doc.getStyle(ConfigurationValueStyle.INTERNAL_LINK_DEFAULT_REDIRECT.getName());
        } else if (Boolean.FALSE.equals(target.isExisting())) {
          style = doc.getStyle(ConfigurationValueStyle.INTERNAL_LINK_DEFAULT_MISSING.getName());
        }
      }
    }

    // Apply default style for internal links
    if (style == null) {
      style = doc.getStyle(ConfigurationValueStyle.INTERNAL_LINK.getName());
    }

    return style;
  }

  /**
   * Format text in a MediaWikiPane.
   * 
   * @param pane MediaWikiPane to be formatted.
   * @param pageAnalysis Page analysis.
   */
  public final void format(MWPane pane, PageAnalysis pageAnalysis) {
    if (pane == null) {
      return;
    }
    // TODO: Detach document from Pane to speed up formatting
    StyledDocument doc = pane.getStyledDocument();
    //StyledDocument tmpDoc = new DefaultStyledDocument();
    //pane.setDocument(tmpDoc);

    // Format document
    format(doc, pageAnalysis);

    // Attach document again before moving caret
    //pane.setDocument(doc);
    moveCaret(pane);
  }

  /**
   * Format text in a StyleDocument.
   * 
   * @param doc Document to be formatted.
   * @param pageAnalysis Page analysis.
   */
  public abstract void format(StyledDocument doc, PageAnalysis pageAnalysis);

  // ==========================================================================
  // Element management
  // ==========================================================================

  /**
   * @param pane Text pane.
   * @param element Element.
   * @return Start of the first element having the same UUID as element.
   */
  public static int getUUIDStartOffset(JTextPane pane, Element element) {
    if (element == null) {
      return Integer.MIN_VALUE;
    }
    if (pane == null) {
      return element.getStartOffset();
    }
    Object uuid = element.getAttributes().getAttribute(ATTRIBUTE_UUID);
    if (uuid == null) {
      return element.getStartOffset();
    }
    int startOffset = element.getStartOffset();
    while (startOffset > 0) {
      Element tmpElement = pane.getStyledDocument().getCharacterElement(startOffset - 1);
      if ((tmpElement == null) || (tmpElement.getAttributes() == null)) {
        return startOffset;
      }
      if (!uuid.equals(tmpElement.getAttributes().getAttribute(ATTRIBUTE_UUID))) {
        return startOffset;
      }
      startOffset = tmpElement.getStartOffset();
    }
    return 0;
  }

  /**
   * @param pane Text pane.
   * @param element Element.
   * @return End of the last element having the same UUID as element.
   */
  public static int getUUIDEndOffet(JTextPane pane, Element element) {
    if (element == null) {
      return Integer.MAX_VALUE;
    }
    if (pane == null) {
      return element.getEndOffset();
    }
    Object uuid = element.getAttributes().getAttribute(ATTRIBUTE_UUID);
    if (uuid == null) {
      return element.getEndOffset();
    }
    int endOffset = element.getEndOffset();
    int length = pane.getText().length();
    while (endOffset < length) {
      Element tmpElement = pane.getStyledDocument().getCharacterElement(endOffset);
      if ((tmpElement == null) || (tmpElement.getAttributes() == null)) {
        return endOffset;
      }
      if (!uuid.equals(tmpElement.getAttributes().getAttribute(ATTRIBUTE_UUID))) {
        return endOffset;
      }
      endOffset = tmpElement.getEndOffset();
    }
    return length;
  }
  // ==========================================================================
  // Document management
  // ==========================================================================

  /**
   * @return A styled document that can be used to format a MWPane.
   */
  public static StyledDocument createDocument() {
    initializeStyles();
    StyledDocument document = new DefaultStyledDocument(styleContext);
    return document;
  }

  // ==========================================================================
  // Style management
  // ==========================================================================

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

        // Style for category
        Style categoryStyle = addStyle(
            ConfigurationValueStyle.CATEGORY, rootStyle, false);
        categoryStyle.addAttribute(ATTRIBUTE_OCCURRENCE, Boolean.FALSE);

        // Style for comment
        Style commentStyle = addStyle(
            ConfigurationValueStyle.COMMENTS, rootStyle, false);
        commentStyle.addAttribute(ATTRIBUTE_OCCURRENCE, Boolean.FALSE);

        // Style for DEFAULTSORT
        Style defaultsortStyle = addStyle(
            ConfigurationValueStyle.DEFAULTSORT, rootStyle, false);
        defaultsortStyle.addAttribute(ATTRIBUTE_OCCURRENCE, Boolean.FALSE);

        // Style for external link
        Style externalLinkStyle = addStyle(
            ConfigurationValueStyle.EXTERNAL_LINK, rootStyle, false);
        externalLinkStyle.addAttribute(ATTRIBUTE_OCCURRENCE, Boolean.FALSE);

        // Style for image
        Style imageStyle = addStyle(
            ConfigurationValueStyle.IMAGE, rootStyle, false);
        imageStyle.addAttribute(ATTRIBUTE_OCCURRENCE, Boolean.FALSE);

        // Style for internal link
        Style internalLinkStyle = addStyle(
            ConfigurationValueStyle.INTERNAL_LINK, rootStyle, false);
        internalLinkStyle.addAttribute(ATTRIBUTE_OCCURRENCE, Boolean.FALSE);

        // Style for missing internal link
        Style internalLinkStyleDefaultMissing = addStyle(
            ConfigurationValueStyle.INTERNAL_LINK_DEFAULT_MISSING, internalLinkStyle, true);
        if (internalLinkStyleDefaultMissing != null) {
          internalLinkStyleDefaultMissing.addAttribute(ATTRIBUTE_OCCURRENCE, Boolean.FALSE);
        }

        // Style for redirect internal link
        Style internalLinkStyleDefaultRedirect = addStyle(
            ConfigurationValueStyle.INTERNAL_LINK_DEFAULT_REDIRECT, internalLinkStyle, true);
        if (internalLinkStyleDefaultRedirect != null) {
          internalLinkStyleDefaultRedirect.addAttribute(ATTRIBUTE_OCCURRENCE, Boolean.FALSE);
        }

        // Style for interwiki link
        Style interwikiLinkStyle = addStyle(
            ConfigurationValueStyle.INTERWIKI_LINK, rootStyle, false);
        interwikiLinkStyle.addAttribute(ATTRIBUTE_OCCURRENCE, Boolean.FALSE);

        // Style for language link
        Style languageLinkStyle = addStyle(
            ConfigurationValueStyle.LANGUAGE_LINK, rootStyle, false);
        languageLinkStyle.addAttribute(ATTRIBUTE_OCCURRENCE, Boolean.FALSE);

        // Style for parameter
        Style parameterStyle = addStyle(
            ConfigurationValueStyle.PROGRAMMING, rootStyle, false);
        parameterStyle.addAttribute(ATTRIBUTE_OCCURRENCE, Boolean.FALSE);

        // Style for reference contents
        Style refStyle = addStyle(
            ConfigurationValueStyle.REFERENCE, rootStyle, false);
        refStyle.addAttribute(ATTRIBUTE_OCCURRENCE, Boolean.FALSE);

        // Style for tag
        Style tagStyle = addStyle(
            ConfigurationValueStyle.TAG, rootStyle, false);
        tagStyle.addAttribute(ATTRIBUTE_OCCURRENCE, Boolean.FALSE);

        // Style for template
        Style templateStyle = addStyle(
            ConfigurationValueStyle.TEMPLATE, rootStyle, false);
        templateStyle.addAttribute(ATTRIBUTE_OCCURRENCE, Boolean.FALSE);

        // Style for title
        Style titleStyle = addStyle(
            ConfigurationValueStyle.TITLE, rootStyle, false);
        titleStyle.addAttribute(ATTRIBUTE_OCCURRENCE, Boolean.FALSE);

        // Style for disambiguation link 
        Style internalLinkDabStyle = addStyle(
            ConfigurationValueStyle.INTERNAL_LINK_DAB, rootStyle, false);
        internalLinkDabStyle.addAttribute(ATTRIBUTE_TYPE, VALUE_DISAMBIGUATION_LINK);

        // Style for normal internal link
        Style internalLinkNormalStyle = addStyle(
            ConfigurationValueStyle.INTERNAL_LINK_NORMAL, rootStyle, false);
        internalLinkNormalStyle.addAttribute(ATTRIBUTE_TYPE, VALUE_NORMAL_LINK);

        // Style for redirect link
        Style internalLinkRedirectStyle = addStyle(
            ConfigurationValueStyle.INTERNAL_LINK_REDIRECT, rootStyle, false);
        internalLinkRedirectStyle.addAttribute(ATTRIBUTE_TYPE, VALUE_REDIRECT_LINK);

        // Style for missing link
        Style internalLinkMissingStyle = addStyle(
            ConfigurationValueStyle.INTERNAL_LINK_MISSING, rootStyle, false);
        internalLinkMissingStyle.addAttribute(ATTRIBUTE_TYPE, VALUE_MISSING_LINK);

        // Style for disambiguation template
        Style templateDabStyle = addStyle(
            ConfigurationValueStyle.TEMPLATE_DAB, rootStyle, false);
        templateDabStyle.addAttribute(ATTRIBUTE_TYPE, VALUE_DISAMBIGUATION_TEMPLATE);

        // Style for normal template
        Style templateNormalStyle = addStyle(
            ConfigurationValueStyle.TEMPLATE_NORMAL, rootStyle, false);
        templateNormalStyle.addAttribute(ATTRIBUTE_TYPE, VALUE_NORMAL_TEMPLATE);

        // Style for help requested
        Style helpRequestedStyle = addStyle(
            ConfigurationValueStyle.HELP_REQUESTED, rootStyle, false);
        helpRequestedStyle.addAttribute(ATTRIBUTE_TYPE, VALUE_HELP_REQUESTED_LINK);

        // Style for CheckWiki error
        Style checkWikiErrorStyle = addStyle(
            ConfigurationValueStyle.CHECK_WIKI_ERROR, rootStyle, false);
        checkWikiErrorStyle.addAttribute(ATTRIBUTE_TYPE, VALUE_CHECK_WIKI_ERROR);

        // Style for CheckWiki warning
        Style checkWikiWarningStyle = addStyle(
            ConfigurationValueStyle.CHECK_WIKI_WARNING, rootStyle, false);
        checkWikiWarningStyle.addAttribute(ATTRIBUTE_TYPE, VALUE_CHECK_WIKI_WARNING);

        // Style for CheckWiki OK
        Style checkWikiOkStyle = addStyle(
            ConfigurationValueStyle.CHECK_WIKI_OK, rootStyle, false);
        checkWikiOkStyle.addAttribute(ATTRIBUTE_TYPE, VALUE_CHECK_WIKI_OK);
        checkWikiOkStyle.addAttribute(ATTRIBUTE_OCCURRENCE, Boolean.FALSE);

        stylesInitialized = true;
      }
    }
  }

  /**
   * @param defaultStyle Default style.
   * @param rootStyle Root style.
   * @return New style.
   */
  private static Style addStyle(
      ConfigurationValueStyle defaultStyle, Style rootStyle,
      boolean onlyEnabled) {
    if (defaultStyle == null) {
      return null;
    }

    // Check that style should be created
    Configuration config = Configuration.getConfiguration();
    ConfigurationValueStyle.StyleProperties configStyle = config.getStyle(defaultStyle);
    if (configStyle == null) {
      return null;
    }
    if (onlyEnabled && !configStyle.getEnabled()) {
      return null;
    }

    // Create style
    Style newStyle = styleContext.addStyle(defaultStyle.getName(), rootStyle);
    formatStyleForeground(newStyle, config, configStyle);
    formatStyleBackground(newStyle, config, configStyle);
    formatStyleBold(newStyle, config, configStyle);
    formatStyleItalic(newStyle, config, configStyle);
    formatStyleUnderline(newStyle, config, configStyle);
    formatStyleStrikeThrough(newStyle, config, configStyle);
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
