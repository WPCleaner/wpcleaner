/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2019  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.gui.swing.deadlink;

import java.io.IOException;
import java.net.SocketException;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.text.JTextComponent;

import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpConnectionManager;
import org.apache.commons.httpclient.HttpMethod;
import org.apache.commons.httpclient.HttpStatus;
import org.apache.commons.httpclient.MultiThreadedHttpConnectionManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.HttpUtils;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementExternalLink;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.i18n.GT;


/**
 * Worker for checking dead links in articles.
 */
public class DeadLinkWorker extends BasicWorker {

  /** Logs */
  private final Logger log = LoggerFactory.getLogger(DeadLinkWorker.class);

  /** List of pages */
  private final List<Page> pages;

  /** Text pane */
  private final JTextComponent textPane;

  /** List of dead links */
  private List<DeadLink> errors; 

  /**
   * @param wiki Wiki.
   * @param window Window
   * @param pages List of pages to check.
   * @param textPane Text pane.
   */
  public DeadLinkWorker(
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
        DeadLinkWindow.createDeadLinkWindow(getWikipedia(), errors, textPane);
      } else {
        if (getWindow() != null) {
          getWindow().displayWarning(GT._T("No dead links were found."));
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
      HttpConnectionManager manager = new MultiThreadedHttpConnectionManager();
      HttpClient client = new HttpClient(manager);
      Map<String, DeadLink> checkedLinks = new HashMap<>();
      for (Page page : pages) {
        if (!shouldContinue()) {
          return errors;
        }
        setText(GT._T("Analyzing {0}", page.getTitle()));
        List<PageElementExternalLink> links = page.getAnalysis(page.getContents(), false).getExternalLinks();
        if (links != null) {
          for (PageElementExternalLink link : links) {
            String url = link.getLink();
            DeadLink deadLink = null;
            if (checkedLinks.containsKey(url)) {
              deadLink = checkedLinks.get(url);
              if (deadLink != null) {
                deadLink = new DeadLink(page.getTitle(), link, deadLink.getStatus());
              }
            } else {
              setText(GT._T("Analyzing {0}", url));
              int count = 0;
              boolean retry = true;
              while (retry && (count < 3)) {
                retry = false;
                deadLink = null;
                HttpMethod method = null;
                try {
                  method = HttpUtils.createHttpHeadMethod(url, null);
                  int questionIndex = url.indexOf('?');
                  if (questionIndex > 0) {
                    method.setQueryString(url.substring(questionIndex + 1));
                  }
                  method.getParams().setSoTimeout(30000);
                  method.setRequestHeader("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3");
                  method.setRequestHeader("Accept-Encoding", "gzip, deflate");
                  method.setRequestHeader("Accept-Language", "en-US,en");
                  method.setRequestHeader("Cache-Control", "no-cache");
                  method.setRequestHeader("Connection", "keep-alive");
                  method.setRequestHeader("Pragma", "no-cache");
                  method.setRequestHeader("Upgrade-Insecure-Requests", "1");
                  //method.setRequestHeader("Content-Type", "text/html");
                  method.setRequestHeader("User-Agent", "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/76.0.3809.100 Safari/537.36");
                  int statusCode = client.executeMethod(method);
                  if (statusCode != HttpStatus.SC_OK) {
                    deadLink = new DeadLink(page.getTitle(), link, statusCode);
                  }
                } catch (IOException e) {
                  if (e instanceof UnknownHostException) {
                    deadLink = new DeadLink(page.getTitle(), link, GT._T("Unknown host {0}", e.getMessage()));
                  } else if (e instanceof SocketException) {
                    log.warn("{} when accessing {}: {}", e.getClass().getSimpleName(), url, e.getMessage());
                    deadLink = new DeadLink(page.getTitle(), link, e.getClass().getSimpleName() + ": " + e.getMessage());
                    retry = true;
                    count++;
                  } else {
                    log.error("{} when accessing {}: {}", e.getClass().getSimpleName(), url, e.getMessage());
                    deadLink = new DeadLink(page.getTitle(), link, e.getClass().getSimpleName() + ": " + e.getMessage());
                  }
                } catch (IllegalStateException e) {
                  log.error("{} when accessing {}: {}", e.getClass().getSimpleName(), url, e.getMessage());
                  deadLink = new DeadLink(page.getTitle(), link, e.getMessage());
                } finally {
                  if (method != null) {
                    method.releaseConnection();
                  }
                }
              }
            }
            if (deadLink != null) {
              errors.add(deadLink);
            }
            if (!checkedLinks.containsKey(url)) {
              checkedLinks.put(url, deadLink);
            }
          }
        }
      }
    } catch (APIException e) {
      return e;
    }
    return errors;
  }
}
