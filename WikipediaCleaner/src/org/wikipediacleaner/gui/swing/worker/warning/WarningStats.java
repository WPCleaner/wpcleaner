/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2022  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.gui.swing.worker.warning;

import java.util.ArrayList;
import java.util.List;

import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;

/** Bean for holding statistics. */
public class WarningStats {

  /**
   * Count of analyzed pages.
   */
  private int analyzedPagesCount;

  /**
   * List of updated pages.
   */
  private List<Page> updatedPages;

  /**
   * Count of warnings that have been removed.
   */
  private int removedWarningsCount;

  /**
   * Count of links.
   */
  private int linksCount;

  public WarningStats() {
    analyzedPagesCount = 0;
    updatedPages = new ArrayList<>();
  }

  void addAnalyzedPage(Page page) {
    if (page != null) {
      analyzedPagesCount++;
    }
  }

  public int getAnalyedPagesCount() {
    return analyzedPagesCount;
  }

  void addUpdatedPage(Page page) {
    if (page != null) {
      updatedPages.add(page);
    }
  }

  public List<Page> getUpdatedPages() {
    return updatedPages;
  }

  public int getUpdatedPagesCount() {
    return (updatedPages != null) ? updatedPages.size() : 0;
  }

  public int getRemovedWarningsCount() {
    return removedWarningsCount;
  }

  void addRemovedWarning(Page page) {
    if (page != null) {
      removedWarningsCount++;
    }
  }

  public int getLinksCount() {
    return linksCount;
  }

  void addLinks(Page page, int count) {
    if (page != null) {
      linksCount += count;
    }
  }

  /**
   * Display statistics.
   * 
   * @param window Window.
   * @param stats Statistics.
   * @param startTime Start time.
   */
  public static void displayStats(
      BasicWindow window,
      WarningStats stats, long startTime) {
    if (window == null) {
      return;
    }
    long endTime = System.currentTimeMillis();
    StringBuilder message = new StringBuilder();
    message.append(GT.__(
        "{0} page has been analyzed.",
        "{0} pages have been analyzed.",
        stats.getAnalyedPagesCount(), Integer.toString(stats.getAnalyedPagesCount())));
    message.append("\n");
    message.append(GT.__(
        "Warning has been updated on {0} page.",
        "Warnings have been updated on {0} pages.",
        stats.getUpdatedPagesCount(), Integer.toString(stats.getUpdatedPagesCount())));
    message.append("\n");
    message.append(GT.__(
        "Warning has been removed on {0} page.",
        "Warnings have been removed on {0} pages.",
        stats.getRemovedWarningsCount(), Integer.toString(stats.getRemovedWarningsCount())));
    message.append("\n");
    message.append(GT.__(
        "{0} still needs to be fixed.",
        "{0} still need to be fixed.",
        stats.getLinksCount(), Integer.toString(stats.getLinksCount())));
    message.append("\n");
    long time = (endTime - startTime) / 1000;
    message.append(GT.__(
        "It took {0} second", "It took {0} seconds", time, Long.toString(time)));
    Utilities.displayInformationMessage(
        window.getParentComponent(), message.toString());
  }
}