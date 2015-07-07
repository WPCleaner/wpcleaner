/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing;

import java.awt.Component;
import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.gui.swing.bot.BotToolsWindow;
import org.wikipediacleaner.gui.swing.checkwiki.CheckWikiWindow;
import org.wikipediacleaner.gui.swing.options.OptionsWindow;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueInteger;


/**
 * A controller for running other windows. 
 */
public class Controller {

  /**
   * Run a full analysis on one page.
   * 
   * @param page Page.
   * @param knownPages Pages already loaded.
   * @param wikipedia Wikipedia
   */
  public static void runFullAnalysis(
      String page, List<Page> knownPages, EnumWikipedia wikipedia) {
    if (page != null) {
      OnePageAnalysisWindow.createAnalysisWindow(page, knownPages, wikipedia);
    }
  }

  /**
   * Run a full analysis on several pages.
   * 
   * @param parentComponent Parent component.
   * @param pages Array of pages.
   * @param knownPages Pages already loaded.
   * @param wikipedia Wikipedia
   */
  public static void runFullAnalysis(
      Component parentComponent, Object[] pages,
      List<Page> knownPages,
      EnumWikipedia wikipedia) {
    if (pages == null) {
      return;
    }
    int pagesCount = getCountOfPages(parentComponent, pages);
    for (int i = 0; i < pagesCount; i++) {
      Object page = pages[i];
      if (page != null) {
        runFullAnalysis(page.toString(), knownPages, wikipedia);
      }
    }
  }

  /**
   * Run a disambiguation analysis on one page.
   * 
   * @param page Page.
   * @param wikipedia Wikipedia
   */
  public static void runDisambiguationAnalysis(String page, EnumWikipedia wikipedia) {
    if (page != null) {
      DisambiguationWindow.createDisambiguationWindow(page, wikipedia);
    }
  }

  /**
   * Run a disambiguation analysis on several pages.
   * 
   * @param parentComponent Parent component.
   * @param pages Array of pages.
   * @param wikipedia Wikipedia
   */
  public static void runDisambiguationAnalysis(
      Component parentComponent, Object[] pages, EnumWikipedia wikipedia) {
    if (pages == null) {
      return;
    }
    int pagesCount = getCountOfPages(parentComponent, pages);
    for (int i = 0; i < pagesCount; i++) {
      Object page = pages[i];
      if (page != null) {
        runDisambiguationAnalysis(page.toString(), wikipedia);
      }
    }
  }

  /**
   * Run automatic fixing.
   * 
   * @param pages List of pages.
   * @param referencePage Page.
   * @param wikipedia Wikipedia
   */
  public static void runAutomaticFixing(
      Collection<Page> pages,
      Page referencePage, EnumWikipedia wikipedia) {
    if (pages != null) {
      AutomaticFixingWindow.createAutomaticFixingWindow(pages, referencePage, wikipedia);
    }
  }

  /**
   * Open the Page Comments window for a page.
   * 
   * @param page Page.
   * @param wikipedia Wikipedia.
   */
  public static void runPageComments(
      Page          page,
      EnumWikipedia wikipedia) {
    if (page != null) {
      PageCommentsWindow.createPageCommentsWindow(page, wikipedia);
    }
  }

  /**
   * Open the Page Comments window for several pages.
   * 
   * @param pages List of pages.
   * @param wikipedia Wikipedia.
   */
  public static void runPageComments(
      Collection<Page> pages,
      EnumWikipedia    wikipedia) {
    for (Page p : pages) {
      runPageComments(p, wikipedia);
    }
  }

  /**
   * Run Check Wiki project.
   * 
   * @param wikipedia Wikipedia
   */
  public static void runCheckWikiProject(EnumWikipedia wikipedia) {
    CheckWikiWindow.createCheckWikiProjectWindow(wikipedia);
  }

  /**
   * Run Bot tools.
   * 
   * @param wikipedia Wikipedia
   */
  public static void runBotTools(EnumWikipedia wikipedia) {
    BotToolsWindow.createBotToolsWindow(wikipedia);
  }

  /**
   * Monitor recent changes.
   * 
   * @param wiki Wiki.
   */
  public static void runMonitorRC(EnumWikipedia wiki) {
    MonitorRCWindow.createMonitorRCWindow(wiki);
  }

  /**
   * Open the options window.
   */
  public static void runOptions() {
    OptionsWindow.createOptionsWindow();
  }

  /**
   * Open the about window.
   */
  public static void runAbout() {
    AboutWindow.createAboutWindow();
  }

  /**
   * Run a new section Window.
   * 
   * @param page Page.
   * @param articleText Text of the article.
   * @param articleTitle Title of the article.
   * @param wikipedia Wikipedia
   */
  public static void runNewSection(
      Page page,
      String articleText, String articleTitle,
      EnumWikipedia wikipedia) {
    if ((page != null) && (wikipedia != null)) {
      NewSectionWindow.createNewSectionWindow(page, articleText, articleTitle, wikipedia);
    }
  }

  /**
   * Open the Expand Templates / Preview window.
   * 
   * @param title Page title.
   * @param text Page text.
   * @param showExpand Flag indicating if the Expand Templates part should be visible.
   * @param showPreview Flag indicating if the Preview part should be visible.
   * @param wikipedia Wikipedia
   */
  public static void runExpandTemplates(
      String title, String text,
      boolean showExpand, boolean showPreview,
      EnumWikipedia wikipedia) {
    if ((title != null) && (text != null)) {
      PreviewWindow.createExpandTemplatesWindow(
          title, text,
          showExpand, showPreview,
          wikipedia);
    }
  }

  /**
   * @param parentComponent Parent component.
   * @param pages Array of pages.
   * @return Number of pages to analyze.
   */
  private static int getCountOfPages(Component parentComponent, Object[] pages) {
    if (pages == null) {
      return 0;
    }
    int pagesCount = pages.length;
    Configuration config = Configuration.getConfiguration();
    int maxPagesCount = config.getInt(
        null,
        ConfigurationValueInteger.MAXIMUM_PAGES);
    if (pagesCount > maxPagesCount) {
      Object[] options = new Object[] {
          GT._("Yes, {0} pages", Integer.toString(pagesCount)),
          GT._("No, {0} pages", Integer.toString(maxPagesCount)),
          GT._("Cancel")
      };
      int answer = Utilities.displayQuestion(
          parentComponent, GT._(
              "You have selected {0} pages.\n" +
              "Would you like to analyze more than {1} pages ?",
              new Object[]{ pagesCount, maxPagesCount }),
          options);
      if (answer == 1) {
        pagesCount = maxPagesCount;
      } else if (answer != 0) {
        pagesCount = 0;
      }
    }
    return pagesCount;
  }
}
