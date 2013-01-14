/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2012  Nicolas Vervelle
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

package org.wikipediacleaner.gui.swing.worker;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.ToolServer;
import org.wikipediacleaner.api.check.CheckError;
import org.wikipediacleaner.api.check.CheckErrorPage;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;


/**
 * SwingWorker for automatic Check Wiki fixing.
 */
public class AutomaticCWWorker extends BasicWorker {

  /**
   * Algorithms for which to fix pages.
   */
  private final List<CheckErrorAlgorithm> selectedAlgorithms;

  /**
   * Maximum number of pages.
   */
  private final int max;

  /**
   * List of potential algorithms to fix.
   */
  private final List<CheckErrorAlgorithm> allAlgorithms;

  /**
   * Count of pages fixed.
   */
  private int count;

  /**
   * @param wiki Wiki.
   * @param window Window.
   * @param selectedAlgorithms List of selected algorithms.
   * @param max Maximum number of pages for each algorithm.
   * @param allAlgorithms List of possible algorithms.
   */
  public AutomaticCWWorker(
      EnumWikipedia wiki, BasicWindow window,
      List<CheckErrorAlgorithm> selectedAlgorithms, int max,
      List<CheckErrorAlgorithm> allAlgorithms) {
    super(wiki, window);
    this.selectedAlgorithms = selectedAlgorithms;
    this.max = max;
    this.allAlgorithms = allAlgorithms;
    this.count = 0;
  }

  /** 
   * Compute the value to be returned by the <code>get</code> method. 
   * 
   * @return Object returned by the <code>get</code> method.
   * @see org.wikipediacleaner.gui.swing.basic.BasicWorker#construct()
   */
  @Override
  public Object construct() {
    List<CheckError> errors = new ArrayList<CheckError>();
    try {
      API api = APIFactory.getAPI();
      ToolServer toolServer = APIFactory.getToolServer();
      for (CheckErrorAlgorithm algorithm : selectedAlgorithms) {
        setText(
            GT._("Checking for errors n°{0}", Integer.toString(algorithm.getErrorNumber())) +
            " - " + algorithm.getShortDescriptionReplaced());
        toolServer.retrievePagesForError(algorithm, max, getWikipedia(), errors);
        for (CheckError error : errors) {
          for (int numPage = 0;
              (numPage < error.getPageCount()) && shouldContinue();
              numPage++) {
            Page page = error.getPage(numPage);
            api.retrieveContents(getWikipedia(), Collections.singletonList(page), false);
            PageAnalysis analysis = page.getAnalysis(page.getContents(), true);
            List<CheckErrorPage> errorPages = CheckError.analyzeErrors(allAlgorithms, analysis);
            boolean found = false;
            if (errorPages != null) {
              for (CheckErrorPage errorPage : errorPages) {
                if (algorithm.equals(errorPage.getAlgorithm()) &&
                    errorPage.getErrorFound()) {
                  found = true;
                }
              }
            }
            if (found) {
              String newContents = page.getContents();
              List<CheckErrorAlgorithm> usedAlgorithms = new ArrayList<CheckErrorAlgorithm>();
              for (CheckErrorAlgorithm currentAlgorithm : allAlgorithms) {
                String tmpContents = newContents;
                analysis = page.getAnalysis(tmpContents, true);
                newContents = currentAlgorithm.botFix(analysis);
                if (!newContents.equals(tmpContents)) {
                  usedAlgorithms.add(currentAlgorithm);
                }
              }
              if (!newContents.equals(page.getContents())) {
                StringBuilder comment = new StringBuilder();
                comment.append(getWikipedia().getCWConfiguration().getComment());
                for (CheckErrorAlgorithm usedAlgorithm : usedAlgorithms) {
                  comment.append(" - ");
                  comment.append(usedAlgorithm.getShortDescriptionReplaced());
                }
                setText(GT._("Fixing page {0}", page.getTitle()));
                api.updatePage(
                    getWikipedia(), page, newContents,
                    getWikipedia().createUpdatePageComment(comment.toString(), null, true),
                    false);
                count++;
                for (CheckErrorAlgorithm usedAlgorithm : usedAlgorithms) {
                  toolServer.markPageAsFixed(page, usedAlgorithm.getErrorNumberString());
                }
              }
            } else if (algorithm.isFullDetection()) {
              toolServer.markPageAsFixed(page, algorithm.getErrorNumberString());
            }
          }
        }
      }
    } catch (APIException e) {
      return e;
    }
    return null;
  }

  /**
   * Called on the event dispatching thread (not on the worker thread)
   * after the <code>construct</code> method has returned.
   * 
   * @see org.wikipediacleaner.gui.swing.basic.BasicWorker#finished()
   */
  @Override
  public void finished() {
    super.finished();
    if (getWindow() != null) {
      Utilities.displayInformationMessage(
          getWindow().getParentComponent(),
          GT._("{0} pages have been fixed", Integer.toString(count)));
    }
  }

}
