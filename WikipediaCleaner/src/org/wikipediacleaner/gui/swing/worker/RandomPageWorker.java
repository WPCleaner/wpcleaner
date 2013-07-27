/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.worker;

import java.util.List;

import javax.swing.text.JTextComponent;

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

  private final JTextComponent text;

  /**
   * @param wikipedia Wikipedia.
   * @param window Window.
   * @param text Text component.
   */
  public RandomPageWorker(EnumWikipedia wikipedia, BasicWindow window, JTextComponent text) {
    super(wikipedia, window);
    this.text = text;
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.utils.SwingWorker#construct()
   */
  @Override
  public Object construct() {
    try {
      setText(GT._("Retrieving MediaWiki API"));
      API api = APIFactory.getAPI();
      setText(GT._("Getting random page"));
      List<Page> pages = api.getRandomPages(getWikipedia(), 1, false);
      if (pages.size() > 0) {
        text.setText(pages.get(0).getTitle());
      } else {
        text.setText("");
      }
    } catch (APIException e) {
      return e;
    }
    return null;
  }
}