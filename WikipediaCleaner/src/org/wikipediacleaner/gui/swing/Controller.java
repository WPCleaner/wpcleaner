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
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.page.PageComment;
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

  private static final Logger LOGGER = LoggerFactory.getLogger(Controller.class);

  /**
   * Run a full analysis on one page.
   * 
   * @param page Page.
   * @param knownPages Pages already loaded.
   * @param wikipedia Wikipedia
   */
  public static void runFullAnalysis(
      String page, List<Page> knownPages, EnumWikipedia wikipedia) {
    try {
      if (page != null) {
        OnePageAnalysisWindow.createAnalysisWindow(page, knownPages, wikipedia);
      }
    } catch (Throwable t) {
      LOGGER.error("Controller.runFullAnalysis: error", t);
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
      Component parentComponent, List<Page> pages,
      List<Page> knownPages,
      EnumWikipedia wikipedia) {
    if (pages == null) {
      return;
    }
    try {
      int pagesCount = getCountOfPages(parentComponent, pages);
      for (int i = 0; i < pagesCount; i++) {
        Object page = pages.get(i);
        if (page != null) {
          runFullAnalysis(page.toString(), knownPages, wikipedia);
        }
      }
    } catch (Throwable t) {
      LOGGER.error("Controller.runFullAnalysis: error", t);
    }
  }

  /**
   * Run a disambiguation analysis on one page.
   * 
   * @param page Page.
   * @param wikipedia Wikipedia
   */
  public static void runDisambiguationAnalysis(String page, EnumWikipedia wikipedia) {
    try {
      if (page != null) {
        DisambiguationWindow.createDisambiguationWindow(page, wikipedia);
      }
    } catch (Throwable t) {
      LOGGER.error("Controller.runDisambiguationAnalysis: error", t);
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
      Component parentComponent, List<Page> pages, EnumWikipedia wikipedia) {
    if (pages == null) {
      return;
    }
    try {
      int pagesCount = getCountOfPages(parentComponent, pages);
      for (int i = 0; i < pagesCount; i++) {
        Object page = pages.get(i);
        if (page != null) {
          runDisambiguationAnalysis(page.toString(), wikipedia);
        }
      }
    } catch (Throwable t) {
      LOGGER.error("Controller.runDisambiguationAnalysis: error", t);
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
    try {
      if (pages != null) {
        AutomaticFixingWindow.createAutomaticFixingWindow(pages, referencePage, wikipedia);
      }
    } catch (Throwable t) {
      LOGGER.error("Controller.runAutomaticFixing: error", t);
    }
  }

  /**
   * Open the Page Comments window for a page.
   * 
   * @param page Page.
   * @param comments Existing comments.
   * @param wikipedia Wikipedia.
   */
  public static void runPageComments(
      Page page,
      PageComment comments,
      EnumWikipedia wikipedia) {
    try {
      if (page != null) {
        PageCommentsWindow.createPageCommentsWindow(page, comments, wikipedia);
      }
    } catch (Throwable t) {
      LOGGER.error("Controller.runPageComments: error", t);
    }
  }

  /**
   * Open the Page Comments window for several pages.
   * 
   * @param pages List of pages.
   * @param commentsByPageTitle Existing comments indexed by page title.
   * @param wikipedia Wikipedia.
   */
  public static void runPageComments(
      Collection<Page> pages,
      Map<String, PageComment> commentsByPageTitle,
      EnumWikipedia    wikipedia) {
    for (Page p : pages) {
      runPageComments(p, commentsByPageTitle.get(p.getTitle()), wikipedia);
    }
  }

  /**
   * Run Check Wiki project.
   * 
   * @param wikipedia Wikipedia
   */
  public static void runCheckWikiProject(EnumWikipedia wikipedia) {
    try {
      CheckWikiWindow.createCheckWikiProjectWindow(wikipedia);
    } catch (Throwable t) {
      LOGGER.error("Controller.runCheckWikiProject: error", t);
    }
  }

  /**
   * Run Bot tools.
   * 
   * @param wikipedia Wikipedia
   */
  public static void runBotTools(EnumWikipedia wikipedia) {
    try {
      BotToolsWindow.createBotToolsWindow(wikipedia);
    } catch (Throwable t) {
      LOGGER.error("Controller.runBotTools: error", t);
    }
  }

  /**
   * Monitor recent changes.
   * 
   * @param wiki Wiki.
   */
  public static void runMonitorRC(EnumWikipedia wiki) {
    try {
      MonitorRCWindow.createMonitorRCWindow(wiki);
    } catch (Throwable t) {
      LOGGER.error("Controller.runMonitorRC: error", t);
    }
  }

  /**
   * Open the options window.
   */
  public static void runOptions() {
    try {
      OptionsWindow.createOptionsWindow();
    } catch (Throwable t) {
      LOGGER.error("Controller.runOptions: error", t);
    }
  }

  /**
   * Open the about window.
   */
  public static void runAbout() {
    try {
      AboutWindow.createAboutWindow();
    } catch (Throwable t) {
      LOGGER.error("Controller.runAbout: error", t);
    }
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
    try {
      if ((page != null) && (wikipedia != null)) {
        NewSectionWindow.createNewSectionWindow(page, articleText, articleTitle, wikipedia);
      }
    } catch (Throwable t) {
      LOGGER.error("Controller.runNewSection: error", t);
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
    try {
      if ((title != null) && (text != null)) {
        PreviewWindow.createExpandTemplatesWindow(
            title, text,
            showExpand, showPreview,
            wikipedia);
      }
    } catch (Throwable t) {
      LOGGER.error("Controller.runExpandTemplates: error", t);
    }
  }

  /**
   * @param parentComponent Parent component.
   * @param pages Array of pages.
   * @return Number of pages to analyze.
   */
  private static int getCountOfPages(Component parentComponent, List<Page> pages) {
    if (pages == null) {
      return 0;
    }
    int pagesCount = pages.size();
    Configuration config = Configuration.getConfiguration();
    int maxPagesCount = config.getInt(
        null,
        ConfigurationValueInteger.MAXIMUM_PAGES);
    if (pagesCount > maxPagesCount) {
      Object[] options = new Object[] {
          GT._T("Yes, {0} pages", Integer.toString(pagesCount)),
          GT._T("No, {0} pages", Integer.toString(maxPagesCount)),
          GT._T("Cancel")
      };
      int answer = Utilities.displayQuestion(
          parentComponent, GT._T(
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
