/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.worker;

import java.util.List;

import javax.swing.JComboBox;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.i18n.GT;

/**
 * SwingWorker for getting random page. 
 */
public class RandomPageWorker extends BasicWorker {

  private final JComboBox combo;

  private String title;

  /**
   * @param wikipedia Wikipedia.
   * @param window Window.
   * @param combo Combo box.
   */
  public RandomPageWorker(EnumWikipedia wikipedia, BasicWindow window, JComboBox combo) {
    super(wikipedia, window);
    this.combo = combo;
    this.title = null;
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.utils.SwingWorker#construct()
   */
  @Override
  public Object construct() {
    try {
      setText(GT._T("Retrieving MediaWiki API"));
      API api = APIFactory.getAPI();
      setText(GT._T("Getting random page"));
      List<Page> pages = api.getRandomPages(getWikipedia(), 1, false);
      if (pages.size() > 0) {
        title = pages.get(0).getTitle();
      } else {
        title = "";
      }
    } catch (APIException e) {
      return e;
    }
    return null;
  }

  /**
   * Update combo box.
   * 
   * @see org.wikipediacleaner.gui.swing.basic.BasicWorker#finished()
   */
  @Override
  public void finished() {
    if (title != null) {
      combo.setSelectedItem(title);
    }
    super.finished();
  }
}