/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2019  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.gui.swing.linter;

import java.awt.Component;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JOptionPane;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.linter.LinterError;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;


/**
 * Worker for checking articles with Linter.
 */
public class LinterWorker extends BasicWorker {

  /** List of pages */
  private final List<Page> pages;

  /** List of Linter errors */
  private List<LinterError> errors; 

  /**
   * @param wiki Wiki.
   * @param window Window
   * @param pages List of pages to check.
   */
  public LinterWorker(
      EnumWikipedia wiki, BasicWindow window,
      List<Page> pages) {
    super(wiki, window);
    this.pages = pages;
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.utils.SwingWorker#finished()
   */
  @Override
  public void finished() {
    super.finished();
    Object result = get();
    Component parent = (getWindow() != null) ? getWindow().getParentComponent() : null;
    if (!(result instanceof Throwable)) {
      LinterErrorPanel panel = new LinterErrorPanel(
          getWikipedia().getWikiConfiguration(), errors, null);
      JOptionPane.showMessageDialog(
          parent, panel, GT._T("Errors"),
          JOptionPane.INFORMATION_MESSAGE);
    } else {
      Utilities.displayError(parent, (Throwable) result);
    }
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.utils.SwingWorker#construct()
   */
  @Override
  public Object construct() {
    try {
      errors = new ArrayList<>();
      API api = APIFactory.getAPI();
      setText(GT._T("Retrieving page contents"));
      api.retrieveContents(getWikipedia(), pages, false, false);
      for (Page page : pages) {
        setText(GT._T("Analyzing {0}", page.getTitle()));
        List<LinterError> tmpErrors = ActionLinter.retrieveLinterErrors(
            getWikipedia(), page.getTitle(), page.getContents());
        if (tmpErrors != null) {
          errors.addAll(tmpErrors);
        }
      }
    } catch (APIException e) {
      return e;
    }
    return errors;
  }
}
