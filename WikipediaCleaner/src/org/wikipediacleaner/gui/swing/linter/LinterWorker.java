/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2019  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.gui.swing.linter;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.swing.text.JTextComponent;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.MediaWikiRESTAPI;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.LinterCategory;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.linter.LinterError;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.i18n.GT;


/**
 * Worker for checking articles with Linter.
 */
public class LinterWorker extends BasicWorker {

  /** List of pages */
  private final List<Page> pages;

  /** Text pane */
  private final JTextComponent textPane;

  /** List of Linter errors */
  private List<LinterError> errors; 

  /**
   * @param wiki Wiki.
   * @param window Window
   * @param pages List of pages to check.
   * @param textPane Text pane.
   */
  public LinterWorker(
      EnumWikipedia wiki, BasicWindow window,
      List<Page> pages, JTextComponent textPane) {
    super(wiki, window);
    this.pages = pages;
    this.textPane = textPane;
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.utils.SwingWorker#finished()
   */
  @Override
  public void finished() {
    super.finished();
    Object result = get();
    if (!(result instanceof Throwable)) {
      if ((errors != null) && !errors.isEmpty()) {
        LinterErrorWindow.createLinterErrorWindow(getWikipedia(), errors, null);
      } else {
        if (getWindow() != null) {
          getWindow().displayWarning(GT._T("No linter errors were found."));
        }
      }
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
      if (textPane == null) {
        setText(GT._T("Retrieving page contents"));
        api.retrieveContents(getWikipedia(), pages, false, false);
      } else {
        for (Page page : pages) {
          page.setContents(textPane.getText());
        }
      }
      for (Page page : pages) {
        setText(GT._T("Analyzing {0}", page.getTitle()));
        List<LinterError> tmpErrors = retrieveLinterErrors(
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

  /**
   * Retrieve linter errors for a page.
   * 
   * @param wiki Wiki.
   * @param pageTitle Title of the page.
   * @param pageText Optional text content of the page.
   * @return List of linter errors in the page.
   * @throws APIException
   */
  private List<LinterError> retrieveLinterErrors(
      EnumWikipedia wiki,
      String pageTitle, String pageText) throws APIException {

    // Retrieve list of errors by calling Linter API
    MediaWikiRESTAPI api = APIFactory.getRESTAPI();
    List<LinterError> tmpErrors = api.transformWikitextToLint(wiki, pageTitle, pageText);
    if (tmpErrors == null) {
      return null;
    }

    // Clean up errors
    Iterator<LinterError> itErrors = tmpErrors.iterator();
    while (itErrors.hasNext()) {
      LinterError error = itErrors.next();
      boolean found = false;
      List<LinterCategory> categories = wiki.getWikiConfiguration().getLinterCategories();
      if (categories != null) {
        for (LinterCategory category : wiki.getWikiConfiguration().getLinterCategories()) {
          if (category.getCategory().equals(error.getType())) {
            found = true;
          }
        }
      }
      if (!found) {
        itErrors.remove();
      }
    }

    return tmpErrors;
  }
}
