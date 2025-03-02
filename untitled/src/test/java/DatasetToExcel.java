import org.apache.poi.ss.usermodel.*;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.openqa.selenium.By;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.chrome.ChromeDriver;

import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;


/*
This code was written on 12.15.2024 and the data was determined according to this date.
If there is any change on the site, the code may not work properly and needs to be updated.
*/
public class DatasetToExcel {
    public static void main(String[] args) throws InterruptedException, IOException {
        System.setProperty("webdriver.chrome.driver", "C:\\Seleniumm\\ChromeDriver\\chromedriver.exe");
        WebDriver driver = new ChromeDriver();
        driver.manage().window().maximize();


        Workbook workbook = new XSSFWorkbook();
        Sheet sheet = workbook.createSheet("Footballers");
        Row headerRow = sheet.createRow(0);
        String[] headers = {"Name", "Nationality", "Age", "Height", "Preferred Foot", "Position", "Market Value",
                "Games Played", "First 11 Played", "Minutes Per Match", "Times Played", "Goals Scored",
                "Expected Goals", "Assists", "Expected Assists", "Pass Accuracy", "Key Passes",
                "Successful Tackles", "Successful Interceptions", "Saves", "Clean Sheets", "Yellow Cards"};

        for (int i = 0; i < headers.length; i++) {
            headerRow.createCell(i).setCellValue(headers[i]);
        }

        List<String> footballers = Arrays.asList("Jakub Slowik", "Deniz Ertas", "Yavuz Aygün", "Egemen Aydin",
                "Guilherme Sitya", "Yusuf Erdoğan", "Josip Ćalušić", "Yasir Subasi");

        int rowIndex = 1;

        try {
            for (String footballer : footballers) {
                driver.get("https://www.sofascore.com/tr/");
                Thread.sleep(2000);

                WebElement searchBox = driver.findElement(By.cssSelector("#search-input"));
                searchBox.clear();
                searchBox.sendKeys(footballer);
                Thread.sleep(2000);

                List<WebElement> results = driver.findElements(By.xpath("//span[contains(text(), '" + footballer + "')]"));
                if (!results.isEmpty()) {
                    results.getFirst().click();
                } else {
                    System.out.println("Sonuç bulunamadı: " + footballer);
                    continue;
                }

                Thread.sleep(3000);

                try {
                    String nationality = driver.findElement(By.cssSelector("div[class='Box Flex ggRYVx doveCn'] span")).getText();
                    String age = driver.findElement(By.cssSelector("div[class='Box Flex ggRYVx flkZQO'] div:nth-child(2) div:nth-child(2)")).getText();
                    String height = driver.findElement(By.cssSelector("div[class='Box Flex ggRYVx flkZQO'] div:nth-child(3) div:nth-child(2)")).getText();
                    String preferredFoot = driver.findElement(By.cssSelector("div[class='Box Flex ggRYVx flkZQO'] div:nth-child(4) div:nth-child(2)")).getText();
                    String Position = driver.findElement(By.cssSelector("div[class='Box oWZdE'] div[class='Text beCNLk']")).getText();

                    String Market_Value = driver.findElement(By.cssSelector(".Text.imGAlA")).getText();
                    JavascriptExecutor js = (JavascriptExecutor) driver;
                    js.executeScript("window.scrollBy(0,1000)");
                    Thread.sleep(2000);


                    String Games_Played = driver.findElement(By.cssSelector("body > div:nth-child(1) > main:nth-child(4) > div:nth-child(2) > div:nth-child(1) > div:nth-child(2) > div:nth-child(2) > div:nth-child(2) > div:nth-child(2) > div:nth-child(4) > div:nth-child(2) > div:nth-child(1) > span:nth-child(2)")).getText();
                    String First_11_Played = driver.findElement(By.cssSelector("body > div:nth-child(1) > main:nth-child(4) > div:nth-child(2) > div:nth-child(1) > div:nth-child(2) > div:nth-child(2) > div:nth-child(2) > div:nth-child(2) > div:nth-child(4) > div:nth-child(2) > div:nth-child(2) > span:nth-child(2)")).getText();
                    String Minutes_Per_Match = driver.findElement(By.cssSelector("body > div:nth-child(1) > main:nth-child(4) > div:nth-child(2) > div:nth-child(1) > div:nth-child(2) > div:nth-child(2) > div:nth-child(2) > div:nth-child(2) > div:nth-child(4) > div:nth-child(2) > div:nth-child(3) > span:nth-child(2)")).getText();
                    String Times_Played = driver.findElement(By.cssSelector("div:nth-child(4) div:nth-child(2) div:nth-child(4) span:nth-child(2)")).getText();
                    String Goals_Scored = driver.findElement(By.cssSelector("div[class='Box kUNcqi sc-91097bb0-2 jwUHH'] div:nth-child(5) div:nth-child(2) div:nth-child(1) span:nth-child(2)")).getText();
                    String Expected_Goals = driver.findElement(By.cssSelector("div[class='Box kUNcqi sc-91097bb0-2 jwUHH'] div:nth-child(5) div:nth-child(2) div:nth-child(2) span:nth-child(2)")).getText();

                    Thread.sleep(2000);
                    js.executeScript("window.scrollBy(0,1000)");

                    String Assists = driver.findElement(By.cssSelector("div[class='Box kUNcqi sc-91097bb0-2 jwUHH'] div:nth-child(6) div:nth-child(2) div:nth-child(1) span:nth-child(2)")).getText();
                    String Expected_Assists = driver.findElement(By.cssSelector("body > div:nth-child(1) > main:nth-child(4) > div:nth-child(2) > div:nth-child(1) > div:nth-child(2) > div:nth-child(2) > div:nth-child(2) > div:nth-child(2) > div:nth-child(6) > div:nth-child(2) > div:nth-child(2) > span:nth-child(2)")).getText();
                    String Pas_Accuracy = driver.findElement(By.cssSelector("div:nth-child(6) div:nth-child(2) div:nth-child(6) span:nth-child(2)")).getText();
                    String Key_Pass = driver.findElement(By.cssSelector("div:nth-child(6) div:nth-child(2) div:nth-child(5) span:nth-child(2)")).getText();
                    String Succesfull_Tackles_per_Match = driver.findElement(By.cssSelector("div[class='Box kUNcqi sc-91097bb0-2 jwUHH'] div:nth-child(7) div:nth-child(2) div:nth-child(2) span:nth-child(2)")).getText();
                    String Succesfull_Interceptions_per_Match = driver.findElement(By.cssSelector("div[class='Box kUNcqi sc-91097bb0-2 jwUHH'] div:nth-child(7) div:nth-child(2) div:nth-child(4) span:nth-child(2)")).getText();
                    String Saves;
                    if (Objects.equals(Position, "K")){
                        Saves = driver.findElement(By.cssSelector("div:nth-child(5) div:nth-child(2) div:nth-child(8) span:nth-child(2)")).getText();
                    }
                    else {
                        Saves = "0";
                    }

                    String Clean_Sheets;
                    if (Objects.equals(Position, "K")){
                        Clean_Sheets = driver.findElement(By.cssSelector("div[class='Box kUNcqi sc-91097bb0-2 jwUHH'] div:nth-child(8) div:nth-child(2) div:nth-child(1) span:nth-child(2)")).getText();
                    }
                    else {
                        Clean_Sheets = "0";
                    }

                    js.executeScript("window.scrollBy(0,1000)");

                    String Yellow_Cards = driver.findElement(By.cssSelector("div[class='Box kUNcqi sc-91097bb0-2 jwUHH'] div:nth-child(10) div:nth-child(2) div:nth-child(1) span:nth-child(2)")).getText();



                    Row row = sheet.createRow(rowIndex++);
                    row.createCell(0).setCellValue(footballer);
                    row.createCell(1).setCellValue(nationality);
                    row.createCell(2).setCellValue(age);
                    row.createCell(3).setCellValue(height);
                    row.createCell(4).setCellValue(preferredFoot);
                    row.createCell(5).setCellValue(Position);
                    row.createCell(6).setCellValue(Market_Value);
                    row.createCell(7).setCellValue(Games_Played);
                    row.createCell(8).setCellValue(First_11_Played);
                    row.createCell(9).setCellValue(Minutes_Per_Match);
                    row.createCell(10).setCellValue(Times_Played);
                    row.createCell(11).setCellValue(Goals_Scored);
                    row.createCell(12).setCellValue(Expected_Goals);
                    row.createCell(13).setCellValue(Assists);
                    row.createCell(14).setCellValue(Expected_Assists);
                    row.createCell(15).setCellValue(Pas_Accuracy);
                    row.createCell(16).setCellValue(Key_Pass);
                    row.createCell(17).setCellValue(Succesfull_Tackles_per_Match);
                    row.createCell(18).setCellValue(Succesfull_Interceptions_per_Match);
                    row.createCell(19).setCellValue(Clean_Sheets);

                    System.out.println("nationality: " + nationality);
                    System.out.println("age: " + age);
                    System.out.println("height: " + height);
                    System.out.println("preferredFoot: " + preferredFoot);
                    if (Objects.equals(Position, "F")){
                        System.out.println("Position: Forward");
                    }
                    if (Objects.equals(Position, "K")){
                        System.out.println("Position: Gaolkeeper");
                    }
                    if (Objects.equals(Position, "D")){
                        System.out.println("Position: Defender");
                    }
                    if (Objects.equals(Position, "OS")){
                        System.out.println("Position: Midfielder");
                    }
                    System.out.println("Market Value: " + Market_Value);
                    System.out.println("Games Played: " + Games_Played);
                    System.out.println("First_11_Played: " + First_11_Played);
                    System.out.println("Minutes_Per_Match: " + Minutes_Per_Match);
                    System.out.println("Times_Played: " + Times_Played);
                    System.out.println("Goals_Scored: " + Goals_Scored);
                    System.out.println("Expected_Goals: " + Expected_Goals);
                    System.out.println("Assists: " + Assists);
                    System.out.println("Expected_Assists: " + Expected_Assists);
                    System.out.println("Pas_Accuracy: " + Pas_Accuracy);
                    System.out.println("Key_Pass: " + Key_Pass);
                    System.out.println("Succesfull_Tackles_per_Match: " + Succesfull_Tackles_per_Match);
                    System.out.println("Succesfull_Interceptions_per_Match: " + Succesfull_Interceptions_per_Match);
                    System.out.println("Saves: " + Saves);
                    System.out.println("Clean_Sheets: " + Clean_Sheets);
                    System.out.println("Yellow_Cards: " + Yellow_Cards);

                } catch (Exception e) {
                    System.out.println("Bilgiler çekilirken hata oluştu: " + footballer);
                }
            }
        } finally {
            driver.quit();


            try (FileOutputStream fileOut = new FileOutputStream("Footballers.xlsx")) {
                workbook.write(fileOut);
            }

            workbook.close();
            System.out.println("Excel dosyası başarıyla oluşturuldu");
        }
    }
}
