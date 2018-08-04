/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2016  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.dump;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.io.Reader;

import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.apache.commons.compress.compressors.bzip2.BZip2CompressorInputStream;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.xml.sax.InputSource;


/**
 * Wiki dumps processor.
 */
public class DumpProcessor {

  /** Logger */
  private final Logger log = LoggerFactory.getLogger(DumpProcessor.class);

  /** Page processor */
  private PageProcessor pageProcessor;

  /**
   * Create a wiki dumps processor.
   * 
   * @param pageProcessor Page processor.
   */
  public DumpProcessor(PageProcessor pageProcessor) {
    this.pageProcessor = pageProcessor;
  }

  /**
   * Process a wiki dump.
   * 
   * @param file File containing the wiki dump.
   */
  public void processDump(File file) {
    if (file == null) {
      return;
    }
    FileInputStream fis = null;
    BufferedInputStream bis = null;
    BZip2CompressorInputStream bzis = null;
    try {
      SAXParserFactory factory = SAXParserFactory.newInstance();
      SAXParser parser = factory.newSAXParser();
      fis = new FileInputStream(file);
      bis = new BufferedInputStream(fis);
      bzis = new BZip2CompressorInputStream(bis);
      Reader reader = new InputStreamReader(bzis, "UTF-8");
      InputSource is = new InputSource(reader); 
      DumpHandler dh = new DumpHandler();
      dh.setPageProcessor(pageProcessor);
      parser.parse(is, dh);
    } catch (Exception e) {
      log.error("Error processing dump file", e);
    } finally {
      try {
        if (bzis != null) {
          bzis.close();
        }
        if (bis != null) {
          bis.close();
        }
        if (fis != null) {
          fis.close();
        }
      } catch (Exception e) {
        log.error("Error closing dump file", e);
      }
    }
  }
}
