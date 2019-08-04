/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.component;

import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.awt.font.TextAttribute;
import java.util.HashMap;
import java.util.Properties;

import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.ListCellRenderer;
import javax.swing.border.EmptyBorder;

import org.wikipediacleaner.api.data.InternalLinkCount;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.utils.Configuration;


/**
 * A list cell renderer for Page. 
 */
public class PageListCellRenderer extends JLabel implements ListCellRenderer<Page> {

  private static final long serialVersionUID = 1456336109709806845L;

  private PageAnalysis analysis;

  private boolean showCountOccurrence;
  private boolean showDisambiguation;
  private boolean showMissing;
  private boolean showRedirect;
  private boolean showRedirectBacklinks;
  private Properties pageProperties;

  private final Font missingFont;
  private final Font normalFont;
  private final Font redirectFont;

  public PageListCellRenderer() {
    setOpaque(true);
    setHorizontalAlignment(LEFT);
    setVerticalAlignment(CENTER);
    setBorder(new EmptyBorder(0, 3, 0, 3));
    HashMap<TextAttribute, Boolean> missingAttributes = new HashMap<TextAttribute, Boolean>();
    missingAttributes.put(TextAttribute.STRIKETHROUGH, Boolean.TRUE /*TextAttribute.STRIKETHROUGH_ON*/);
    missingFont = getFont().deriveFont(missingAttributes);
    normalFont = getFont().deriveFont(Font.PLAIN);
    redirectFont = getFont().deriveFont(Font.ITALIC);
    setFont(normalFont);
  }

  /**
   * @param properties Page properties.
   */
  public void setPageProperties(Properties properties) {
    pageProperties = properties;
  }

  /**
   * @param show Flag indicating if the occurrence count is shown.
   */
  public void showCountOccurrence(boolean show) {
    showCountOccurrence = show;
  }

  /**
   * @param show Flag indicating if the disambiguation flag is shown.
   */
  public void showDisambiguation(boolean show) {
    showDisambiguation = show;
  }

  /**
   * @param analysis Page analysis.
   */
  public void setPageAnalysis(PageAnalysis analysis) {
    this.analysis = analysis;
  }

  /**
   * @param show Flag indicating if missing pages are highlighted.
   */
  public void showMissing(boolean show) {
    showMissing = show;
  }

  /**
   * @param show Flag indicating if redirect pages are highlighted.
   */
  public void showRedirect(boolean show) {
    showRedirect = show;
  }

  /**
   * @param show Flag indicating if numbers of pages linking to redirect pages are displayed.
   */
  public void showRedirectBacklinks(boolean show) {
    showRedirectBacklinks = show;
  }

  /* (non-Javadoc)
   * @see javax.swing.ListCellRenderer#getListCellRendererComponent(
   *          javax.swing.JList, java.lang.Object, int, boolean, boolean)
   */
  @Override
  public Component getListCellRendererComponent(
      JList<? extends Page> list,
      Page value,
      @SuppressWarnings("unused") int index,
      boolean isSelected,
      @SuppressWarnings("unused") boolean cellHasFocus) {

    // Retrieve data
    String pageName = (value != null) ? value.toString() : "";
    String text = pageName;
    Boolean disambiguation = null;
    Boolean exist = null;
    boolean redirect = false;
    InternalLinkCount count = null;
    if (value != null) {
      Page pageElement = value;
      pageName = pageElement.getTitle();
      text = pageName;
      disambiguation = pageElement.isDisambiguationPage();
      exist = pageElement.isExisting();
      count = (analysis != null) ? analysis.getLinkCount(pageElement) : null;
      if (showCountOccurrence &&
          (count != null) &&
          (count.getTotalLinkCount() > 0)) {
        text += " → " + count.getTotalLinkCount(); 
      }
      redirect = pageElement.getRedirects().isRedirect();
      if (redirect && showRedirectBacklinks) {
        Integer backlinks = pageElement.getBacklinksCountInMainNamespace();
        if ((backlinks != null) && (backlinks.intValue() > 0)) {
          text += " ← " + backlinks;
        }
      }
    }

    // Text
    setText(text);

    // Color
    Color background = isSelected ? list.getSelectionBackground() : Color.WHITE;
    Color foreground = isSelected ? list.getSelectionForeground() : list.getForeground();
    if (showDisambiguation) {
      if (disambiguation == null) {
        if (!isSelected) {
          foreground = Color.DARK_GRAY;
        }
      } else if (disambiguation.booleanValue()) {
        if (count == null) {
          foreground = Color.RED;
        } else if ((count.getInternalLinkCount() > 0) || (count.getIncorrectTemplateCount() > 0)) {
          foreground = Color.RED;
        } else if ((count.getHelpNeededCount() > 0)) {
          foreground = Color.ORANGE;
        } else {
          foreground = Color.BLUE;
        }
      }
    } else if (pageProperties != null) {
      String property = pageProperties.getProperty(pageName);
      if (Configuration.VALUE_PAGE_NORMAL.equals(property)) {
        foreground = Color.GREEN;
      } else if (Configuration.VALUE_PAGE_HELP_NEEDED.equals(property)) {
        foreground = Color.ORANGE;
      }
    }
    setBackground(background);
    setForeground(foreground);

    // Font
    if (showMissing && Boolean.FALSE.equals(exist)) {
      setFont(missingFont);
    } else if (showRedirect && redirect) {
      setFont(redirectFont);
    } else {
      setFont(normalFont);
    }

    return this;
  }

}
