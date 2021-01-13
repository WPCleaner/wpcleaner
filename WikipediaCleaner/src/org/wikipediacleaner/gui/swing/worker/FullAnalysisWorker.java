/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.worker;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.MediaWiki;
import org.wikipediacleaner.api.algorithm.AlgorithmError;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithms;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;

/**
 * SwingWorker for reloading the page. 
 */
public class FullAnalysisWorker extends BasicWorker {

  private final Page page;
  private final List<Page> knownPages;
  private final Collection<CheckErrorAlgorithm> algorithms;

  /**
   * @param wikipedia Wikipedia.
   * @param window Window.
   * @param page Page.
   * @param knownPages Known information about some pages.
   * @param algorithms Check Wiki errors.
   */
  public FullAnalysisWorker(
      EnumWikipedia wikipedia, BasicWindow window, Page page,
      List<Page> knownPages, Collection<CheckErrorAlgorithm> algorithms) {
    super(wikipedia, window);
    this.page = page;
    this.knownPages = knownPages;
    this.algorithms = algorithms;
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.utils.SwingWorker#construct()
   */
  @Override
  public Object construct() {
    try {
      MediaWiki mw = MediaWiki.getMediaWikiAccess(this);
      final API api = APIFactory.getAPI();
      EnumWikipedia wiki = getWikipedia();
      mw.retrieveContents(wiki, page, false, false, false, true, false);
      api.retrieveLinks(wiki, page, Namespace.MAIN, knownPages, true, true);

      // Retrieve disambiguation information if not already retrieved
      List<Page> links = new ArrayList<>();
      for (Page link : page.getLinks()) {
        if (link.isDisambiguationPage() == null) {
          links.add(link);
        }
      }
      if (!links.isEmpty()) {
        mw.retrieveDisambiguationInformation(wiki, links, knownPages, true, false, true);
      }

      // Retrieve more information on disambiguation pages
      for (Page link : page.getLinks()) {
        if (Boolean.TRUE.equals(link.isDisambiguationPage())) {
          Iterator<Page> itLink = link.getRedirects().getIteratorWithPage();
          while (itLink.hasNext()) {
            Page link2 = itLink.next();
            if (!link2.getRedirects().isRedirect()) {
              mw.retrieveAllLinks(wiki, link2, null, knownPages, false, false);
            }
            if (link.hasWiktionaryTemplate() &&
                (link.getContents() == null)) {
              mw.retrieveContents(wiki, link2, false, false, false, true, false);
            }
          }
        }
      }

      if (CheckErrorAlgorithms.isAlgorithmActive(wiki, 508)) {
        mw.retrieveAllTemplates(wiki, page, false);
      }
      mw.block(true);
      if (Boolean.FALSE.equals(page.isExisting())) {
        mw.retrieveSimilarPages(wiki, page);
      }
      setText("Analyzing data");
      PageAnalysis analysis = page.getAnalysis(page.getContents(), true);
      AlgorithmError.analyzeErrors(algorithms, analysis, false);
    } catch (APIException e) {
      return e;
    }
    return null;
  }
}