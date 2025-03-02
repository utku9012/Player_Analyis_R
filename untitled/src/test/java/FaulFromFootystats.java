import org.openqa.selenium.By;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.chrome.ChromeDriver;

import java.util.Arrays;
import java.util.List;

/*
This code was written on 12.15.2024 and the data was determined according to this date.
If there is any change on the site, the code may not work properly and needs to be updated.
*/

public class FaulFromFootystats {
    public static void main(String[] args) {

        System.setProperty("webdriver.chrome.driver", "C:\\Seleniumm\\ChromeDriver\\chromedriver.exe");
        WebDriver driver = new ChromeDriver();

        try {

            driver.get("https://footystats.org/clubs/konyaspor-194#");
            driver.manage().window().maximize();
            Thread.sleep(3000);

            driver.findElement(By.cssSelector("div[id='club_content1'] a:nth-child(3)")).click();
            JavascriptExecutor js = (JavascriptExecutor) driver;
            js.executeScript("window.scrollBy(0,350)");
            Thread.sleep(2000);


            List<String> playersToClick = Arrays.asList(
                    "Jakub Slowik", "Deniz Ertas", "Yavuz Aygün", "Egemen Aydin", "Guilherme Sitya", "Yusuf Erdoğan", "Josip Ćalušić", "Yasir Subasi", "Riechedly Bazoer", "Adil Demirbag", "Marko Jevtović", "Filip Damjanović",
                    "Ugurcan Yazgili", "Ufuk Akyol", "Adem Eren Kabak", "Nikola Boranijašević", "Ogulcan Ülgün", "Emmanuel Boateng", "Niko Rak", "Semih Kocatürk", "Danijel Aleksić", "Melih Ibrahimoglu", "Louka Prip", "Alassane Ndao",
                    "Hamidou Keyta", "Blaz Kramer", "Henrique Pedrinho", "Tunahan Tasci", "Umut Nayir", "Melih Bostan"




            );


            for (String playerName : playersToClick) {
                try {

                    String xpath = "//a[text()='" + playerName + "']";
                    WebElement playerLink = driver.findElement(By.xpath(xpath));
                    playerLink.click();
                    Thread.sleep(2000);
                    driver.findElement(By.xpath("//a[@id='sub-nav-cards']")).click();

                    Thread.sleep(2000);
                    String Fouls_Committed = "";
                    String Fouled_Against = "";
                    try{
                         Fouls_Committed = driver.findElement(By.cssSelector("body > div:nth-child(4) > div:nth-child(7) > main:nth-child(1) > section:nth-child(2) > div:nth-child(2) > div:nth-child(2) > div:nth-child(9) > table:nth-child(2) > tbody:nth-child(2) > tr:nth-child(6) > td:nth-child(2)")).getText();
                         Fouled_Against = driver.findElement(By.cssSelector("body > div:nth-child(4) > div:nth-child(7) > main:nth-child(1) > section:nth-child(2) > div:nth-child(2) > div:nth-child(2) > div:nth-child(9) > table:nth-child(2) > tbody:nth-child(2) > tr:nth-child(7) > td:nth-child(2)")).getText();

                    }
                    catch(Exception e){
                        System.out.println("Futbolcu verileri bulunamadi: "+playerName);
                    }
                    

                    driver.navigate().back();
                    Thread.sleep(2000);

                    System.out.println(playerName + "\nFouls_Committed: " + Fouls_Committed);
                    System.out.printf("Fouled_Against: " + Fouled_Against);
                    System.out.println("\n------------------");
                } catch (Exception e) {
                    System.out.println("Futbolcu bulunamadı veya tıklanamadı: " + playerName);
                }
            }

        } catch (InterruptedException e) {
            e.printStackTrace();
        } finally {

            driver.quit();
        }
    }
}
