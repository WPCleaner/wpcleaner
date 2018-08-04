/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.worker;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.i18n.GT;

/**
 * SwingWorker for sending the new section content. 
 */
public class NewSectionWorker extends BasicWorker {

  private final Page page;
  private final String section;
  private final String text;
  private final boolean forceWatch;

  /**
   * @param wikipedia Wikipedia.
   * @param window Window.
   * @param page Page.
   * @param section Section title.
   * @param text Section text.
   * @param forceWatch Force watch.
   */
  public NewSectionWorker(
      EnumWikipedia wikipedia, BasicWindow window, Page page,
      String section, String text, boolean forceWatch) {
    super(wikipedia, window);
    this.page = page;
    this.section = section;
    this.text = text;
    this.forceWatch = forceWatch;
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.utils.SwingWorker#finished()
   */
  @Override
  public void finished() {
    super.finished();
    getWindow().dispose();
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.utils.SwingWorker#construct()
   */
  @Override
  public Object construct() {
    try {
      setText(GT._T("Retrieving MediaWiki API"));
      API api = APIFactory.getAPI();
      setText(GT._T("Adding comment"));
      api.addNewSection(getWikipedia(), page, section, text, true, false, forceWatch);
    } catch (APIException e) {
      return e;
    }
    return null;
  }
}