/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.bot;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Collection;
import java.util.List;

import org.apache.commons.io.FileUtils;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;


/**
 * SwingWorker for automatic Check Wiki fixing on a list of pages extracted from a file.
 */
public class AutomaticFileCWWorker extends AutomaticFixWorker {

  /** File containing the list of pages to fix. */
  private final String path;

  /**
   * @param wiki Wiki.
   * @param window Window.
   * @param path File containing the list of pages to fix.
   * @param selectedAlgorithms List of selected algorithms.
   * @param allAlgorithms List of possible algorithms.
   * @param selectedNamespaces List of selected namespaces.
   * @param extraComment Extra comment.
   * @param saveModifications True if modifications should be saved.
   * @param analyzeNonFixed True if pages that couldn't be fixed should be analyzed.
   */
  public AutomaticFileCWWorker(
      EnumWikipedia wiki, BasicWindow window,
      String path,
      List<CheckErrorAlgorithm> selectedAlgorithms,
      List<CheckErrorAlgorithm> allAlgorithms,
      Collection<Integer> selectedNamespaces,
      String extraComment,
      boolean saveModifications, boolean analyzeNonFixed) {
    super(
        wiki, window,
        selectedAlgorithms, allAlgorithms, selectedNamespaces,
        extraComment, saveModifications, analyzeNonFixed);
    this.path = path;
  }

  /** 
   * Compute the value to be returned by the <code>get</code> method. 
   * 
   * @return Object returned by the <code>get</code> method.
   * @see org.wikipediacleaner.gui.swing.basic.BasicWorker#construct()
   */
  @Override
  public Object construct() {
    try {
      File file = new File(path);
      String fileContent = FileUtils.readFileToString(file, StandardCharsets.UTF_8);
      Page page = DataManager.getPage(getWikipedia(), file.getName(), null, null, null);
      page.setContents(fileContent);
      PageAnalysis analysis = page.getAnalysis(fileContent, true);
      for (PageElementInternalLink link : analysis.getInternalLinks()) {
        if (!shouldContinue()) {
          return null;
        }
        Page tmpPage = DataManager.getPage(getWikipedia(), link.getLink(), null, null, null);
        analyzePage(tmpPage, selectedAlgorithms, null);
      }
    } catch (APIException | IOException e) {
      return e;
    }
    return null;
  }
}
