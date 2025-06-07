/// The main entry of one stylang app.
fn main() -> view {

    let @state @option value: string = none;

    <navigation-stack>
        <center>
            <column>
                // core mod `web3`.
                if web3::is_connected() {
                    <label class="header" text="Sponsor styles-lab"/>
                } else {
                    <label class="header" text="Connect to Etherwallet to start donating"/>
                }

                <row></row>
            </column>
        </center>
    </navigation-stack>
}

@platform
extern fn transfer(target: string, amount: bignum) -> [u8;20];

fn donate(amount: bignum) {
    let tx = transfer("xxxxx",10);
}
