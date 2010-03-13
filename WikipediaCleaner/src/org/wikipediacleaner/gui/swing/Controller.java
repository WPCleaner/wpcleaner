/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2007  Nicolas Vervelle
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

package org.wikipediacleaner.gui.swing;

import java.awt.Component;
import java.util.ArrayList;

import javax.swing.JOptionPane;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.gui.swing.component.MediaWikiPane;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.Configuration;


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
      String page, ArrayList<Page> knownPages, EnumWikipedia wikipedia) {
    if (page != null) {
      AnalysisWindow.createAnalysisWindow(page, knownPages, wikipedia);
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
      ArrayList<Page> knownPages,
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
   * Run a red links analysis.
   * 
   * @param page Page.
   * @param textPane Text pane.
   * @param wikipedia Wikipedia.
   */
  public static void runRedLinksAnalysis(
      Page page,
      MediaWikiPane textPane,
      EnumWikipedia wikipedia) {
    if (page != null) {
      RedLinksWindow.createRedLinksWindow(page, textPane, wikipedia);
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
      Page[]        pages,
      EnumWikipedia wikipedia) {
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
    CheckWikiProjectWindow.createCheckWikiProjectWindow(wikipedia);
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
   * @param editToken Edit token.
   * @param wikipedia Wikipedia
   */
  public static void runNewSection(
      String page,
      String articleText,
      String editToken,
      EnumWikipedia wikipedia) {
    if ((page != null) && (wikipedia != null)) {
      NewSectionWindow.createNewSectionWindow(page, articleText, editToken, wikipedia);
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
        Configuration.INTEGER_MAXIMUM_PAGES,
        Configuration.DEFAULT_MAXIMUM_PAGES);
    if (pagesCount > maxPagesCount) {
      int answer = Utilities.displayYesNoWarning(
          parentComponent, GT._(
              "You have selected {0} pages.\n" +
              "Would you like to analyze the {0} pages ?",
              new Object[]{ pagesCount }));
      if (answer != JOptionPane.YES_OPTION) {
        pagesCount = maxPagesCount;
      }
    }
    return pagesCount;
  }
}
