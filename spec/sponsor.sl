/// The main entry of one stylang app.
fn main() -> view {
    let stat msg = "hello world";

    <navigation-stack>
        <center>
            <column>
                if solidity.is_connected() {
                    <label class.theme="header" text="Sponsor styles-lab"/>
                } else {
                    <label class.theme="header" text="Connect to Etherwallet to start donating"/>
                }
                
                <row>
                    <text-field id="donate" prompt="Donate via ethereum network with a minimum donation of 0.1eth."/>
                    <button icon="assets://ethereum.svg" text="Donate" on-click.solidity="donate(${donate})"/>
                </row>
            </column>
        </center>
    </navigation-stack>
}