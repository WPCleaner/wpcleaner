/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.worker;

import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.InformationWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.i18n.GT;

/**
 * A worker for checking articles.
 */
public class CheckArticleWorker extends BasicWorker {

  private final Collection<Page> pageList;

  /**
   * @param wikipedia Wikipedia.
   * @param window Window.
   * @param pages Pages.
   */
  public CheckArticleWorker(
      EnumWikipedia wikipedia, BasicWindow window,
      List<Page> pages) {
    super(wikipedia, window);
    this.pageList = pages;
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.utils.SwingWorker#construct()
   */
  @Override
  public Object construct() {
    try {
      if (pageList != null) {
        CheckArticleTools tools = new CheckArticleTools(getWikipedia());
        for (Page page : pageList) {
          tools.checkArticle(page);
        }
        String report = tools.getReport();
        InformationWindow.createInformationWindow(GT._T("Analysis"), report, false, getWikipedia());
      }
    } catch (APIException e) {
      return e;
    }
    return null;
  }
}